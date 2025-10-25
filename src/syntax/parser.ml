type t = {
    stream : Token.token_stream
  ; interner : Musi_shared.Interner.t
  ; diags : Musi_shared.Diagnostic.diagnostic_bag ref
}

let make tokens interner =
  {
    stream = Token.make_stream tokens
  ; interner
  ; diags = ref Musi_shared.Diagnostic.empty_bag
  }

let is_trivia = function
  | Token.Whitespace | Token.Newline | Token.LineComment _
  | Token.BlockComment _ ->
    true
  | _ -> false

let collect_trivia t =
  let rec loop acc =
    let tok = Token.curr t.stream in
    if is_trivia tok.kind then (
      Token.advance t.stream;
      loop (tok :: acc))
    else List.rev acc
  in
  loop []

let curr t =
  let _ = collect_trivia t in
  Token.curr t.stream

let advance t =
  Token.advance t.stream;
  let _ = collect_trivia t in
  ()

let expect t kind =
  if (curr t).kind = kind then (
    advance t;
    true)
  else (
    t.diags :=
      Musi_shared.Diagnostic.add
        !(t.diags)
        (Musi_shared.Diagnostic.error "expected token" (curr t).span);
    false)

let parse_separated parse_item sep term t =
  let rec loop acc =
    if (curr t).kind = term || (curr t).kind = Token.Eof then List.rev acc
    else
      let item = parse_item t in
      let acc' = item :: acc in
      if (curr t).kind = sep then (
        advance t;
        loop acc')
      else List.rev acc'
  in
  loop []

let prefix_bp = function Token.Minus | Token.KwNot -> Some 13 | _ -> None

let infix_bp = function
  | Token.Star | Token.Slash -> Some (11, 12)
  | Token.Plus | Token.Minus -> Some (9, 10)
  | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.Eq -> Some (5, 6)
  | _ -> None

let rec parse_expr t : Tree.expr = parse_expr_bp t 0

and parse_expr_bp t min_bp : Tree.expr =
  let lhs =
    match prefix_bp (curr t).kind with
    | Some bp -> parse_prefix_expr t bp
    | None -> parse_primary_expr t
  in
  parse_infix_expr t lhs min_bp

and parse_primary_expr t : Tree.expr =
  let leading = collect_trivia t in
  let tok = Token.curr t.stream in
  Token.advance t.stream;
  match tok.kind with
  | Token.LitInt (s, _) ->
    {
      Tree.kind = Tree.IntLit { value = s }
    ; span = tok.span
    ; leading
    ; trailing = []
    ; typ = None
    ; sym = None
    }
  | Token.LitFloat (s, _) ->
    {
      Tree.kind = Tree.BinLit { value = s }
    ; span = tok.span
    ; leading
    ; trailing = []
    ; typ = None
    ; sym = None
    }
  | Token.LitText sym ->
    {
      Tree.kind = Tree.TextLit { value = sym }
    ; span = tok.span
    ; leading
    ; trailing = []
    ; typ = None
    ; sym = None
    }
  | Token.KwTrue ->
    {
      Tree.kind = Tree.BoolLit { value = true }
    ; span = tok.span
    ; leading
    ; trailing = []
    ; typ = None
    ; sym = None
    }
  | Token.KwFalse ->
    {
      Tree.kind = Tree.BoolLit { value = false }
    ; span = tok.span
    ; leading
    ; trailing = []
    ; typ = None
    ; sym = None
    }
  | Token.Ident sym ->
    {
      Tree.kind = Tree.Ident { name = sym }
    ; span = tok.span
    ; leading
    ; trailing = []
    ; typ = None
    ; sym = None
    }
  | Token.LParen -> parse_paren_expr t tok.span leading
  | Token.LBrace -> parse_block_expr t tok.span leading
  | Token.KwIf -> parse_if_expr t tok.span leading
  | _ ->
    t.diags :=
      Musi_shared.Diagnostic.add
        !(t.diags)
        (Musi_shared.Diagnostic.error "expected expression" tok.span);
    {
      Tree.kind = Tree.Error
    ; span = tok.span
    ; leading
    ; trailing = []
    ; typ = None
    ; sym = None
    }

and parse_prefix_expr t bp : Tree.expr =
  let leading = collect_trivia t in
  let op_tok = Token.curr t.stream in
  Token.advance t.stream;
  let expr = parse_expr_bp t bp in
  let span =
    Musi_shared.Span.make
      (Musi_shared.Span.file op_tok.span)
      (Musi_shared.Span.start op_tok.span)
      (Musi_shared.Span.end_ expr.span)
  in
  {
    Tree.kind = Tree.Unary { op = op_tok.kind; operand = expr }
  ; span
  ; leading
  ; trailing = []
  ; typ = None
  ; sym = None
  }

and parse_infix_expr t lhs min_bp =
  match (curr t).kind with
  | Token.LParen ->
    advance t;
    let args = parse_call_args t in
    let _ = expect t Token.RParen in
    let span =
      Musi_shared.Span.make
        (Musi_shared.Span.file lhs.span)
        (Musi_shared.Span.start lhs.span)
        (Musi_shared.Span.end_ (curr t).span)
    in
    parse_infix_expr
      t
      {
        Tree.kind = Tree.Call { callee = lhs; args }
      ; span
      ; leading = []
      ; trailing = []
      ; typ = None
      ; sym = None
      }
      min_bp
  | _ -> (
    match infix_bp (curr t).kind with
    | Some (lbp, rbp) when lbp >= min_bp ->
      let op = (curr t).kind in
      advance t;
      let rhs = parse_expr_bp t rbp in
      let span =
        Musi_shared.Span.make
          (Musi_shared.Span.file lhs.span)
          (Musi_shared.Span.start lhs.span)
          (Musi_shared.Span.end_ rhs.span)
      in
      parse_infix_expr
        t
        {
          Tree.kind = Tree.Binary { op; lhs; rhs }
        ; span
        ; leading = []
        ; trailing = []
        ; typ = None
        ; sym = None
        }
        min_bp
    | _ -> lhs)

and parse_call_args t = parse_separated parse_expr Token.Comma Token.RParen t

and parse_paren_expr t _start _leading =
  let expr = parse_expr t in
  let _ = expect t Token.RParen in
  expr

and parse_block_expr t start leading : Tree.expr =
  let _ = collect_trivia t in
  let stmts = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let span =
    Musi_shared.Span.make
      (Musi_shared.Span.file start)
      (Musi_shared.Span.start start)
      (Musi_shared.Span.end_ (curr t).span)
  in
  {
    Tree.kind = Tree.Block { stmts }
  ; span
  ; leading
  ; trailing = []
  ; typ = None
  ; sym = None
  }

and parse_if_expr t start leading : Tree.expr =
  let _ = collect_trivia t in
  let cond = parse_expr t in
  let _ = expect t Token.KwThen in
  let _ = expect t Token.LBrace in
  let then_stmts = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let else_br : Tree.expr option =
    if (curr t).kind = Token.KwElse then (
      advance t;
      let _ = expect t Token.LBrace in
      let stmts = parse_block_stmts t in
      let _ = expect t Token.RBrace in
      Some
        {
          Tree.kind = Tree.Block { stmts }
        ; span =
            Musi_shared.Span.make
              (Musi_shared.Span.file start)
              (Musi_shared.Span.start start)
              (Musi_shared.Span.end_ (curr t).span)
        ; leading = []
        ; trailing = []
        ; typ = None
        ; sym = None
        })
    else None
  in
  let span =
    Musi_shared.Span.make
      (Musi_shared.Span.file start)
      (Musi_shared.Span.start start)
      (Musi_shared.Span.end_ (curr t).span)
  in
  let then_br : Tree.expr =
    {
      Tree.kind = Tree.Block { stmts = then_stmts }
    ; span
    ; leading = []
    ; trailing = []
    ; typ = None
    ; sym = None
    }
  in
  {
    Tree.kind = Tree.If { cond; then_br; else_br }
  ; span
  ; leading
  ; trailing = []
  ; typ = None
  ; sym = None
  }

and parse_stmt t : Tree.stmt =
  let leading = collect_trivia t in
  match (Token.curr t.stream).kind with
  | Token.KwLet -> parse_bind_stmt t false leading
  | Token.KwVar -> parse_bind_stmt t true leading
  | Token.KwReturn -> parse_return_stmt t leading
  | Token.KwWhile -> parse_while_stmt t leading
  | _ ->
    let expr = parse_expr_bp t 0 in
    if (curr t).kind = Token.LtMinus then parse_assign_stmt t expr leading
    else
      {
        Tree.kind = Tree.Expr { expr }
      ; span = expr.span
      ; leading
      ; trailing = []
      ; sym = None
      }

and parse_bind_stmt t mut leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  let name =
    match (curr t).kind with
    | Token.Ident sym ->
      advance t;
      sym
    | _ ->
      t.diags :=
        Musi_shared.Diagnostic.add
          !(t.diags)
          (Musi_shared.Diagnostic.error "expected identifier" (curr t).span);
      Musi_shared.Interner.intern t.interner "<error>"
  in
  let ty =
    if (curr t).kind = Token.Colon then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let _ = expect t Token.ColonEq in
  let init = parse_expr t in
  let span =
    Musi_shared.Span.make
      (Musi_shared.Span.file start)
      (Musi_shared.Span.start start)
      (Musi_shared.Span.end_ init.span)
  in
  {
    Tree.kind = Tree.Bind { mut; name; typ = ty; init }
  ; span
  ; leading
  ; trailing = []
  ; sym = None
  }

and parse_assign_stmt t lhs leading : Tree.stmt =
  let start = lhs.span in
  advance t;
  let name =
    match lhs.kind with
    | Tree.Ident { name } -> name
    | _ ->
      t.diags :=
        Musi_shared.Diagnostic.add
          !(t.diags)
          (Musi_shared.Diagnostic.error "expression is not assignable" start);
      Musi_shared.Interner.intern t.interner "<error>"
  in
  let rhs = parse_expr t in
  let span =
    Musi_shared.Span.make
      (Musi_shared.Span.file start)
      (Musi_shared.Span.start start)
      (Musi_shared.Span.end_ rhs.span)
  in
  {
    Tree.kind = Tree.Assign { name; value = rhs }
  ; span
  ; leading
  ; trailing = []
  ; sym = None
  }

and parse_return_stmt t leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  let value =
    if (curr t).kind = Token.Semi then None else Some (parse_expr t)
  in
  let _ = expect t Token.Semi in
  let span =
    match value with
    | Some e ->
      Musi_shared.Span.make
        (Musi_shared.Span.file start)
        (Musi_shared.Span.start start)
        (Musi_shared.Span.end_ e.span)
    | None -> start
  in
  {
    Tree.kind = Tree.Return { value }
  ; span
  ; leading
  ; trailing = []
  ; sym = None
  }

and parse_while_stmt t leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  let cond = parse_expr t in
  let _ = expect t Token.LBrace in
  let body = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let span =
    Musi_shared.Span.make
      (Musi_shared.Span.file start)
      (Musi_shared.Span.start start)
      (Musi_shared.Span.end_ (curr t).span)
  in
  {
    Tree.kind = Tree.While { cond; body }
  ; span
  ; leading
  ; trailing = []
  ; sym = None
  }

and parse_block_stmts t =
  let rec loop acc =
    if (curr t).kind = Token.RBrace || (curr t).kind = Token.Eof then
      List.rev acc
    else
      let stmt = parse_stmt t in
      if (curr t).kind = Token.Semi then advance t;
      loop (stmt :: acc)
  in
  loop []

and parse_ty t : Tree.typ =
  let leading = collect_trivia t in
  let tok = Token.curr t.stream in
  Token.advance t.stream;
  let kind =
    match tok.kind with
    | Token.Ident sym -> Tree.Named { name = sym }
    | _ ->
      t.diags :=
        Musi_shared.Diagnostic.add
          !(t.diags)
          (Musi_shared.Diagnostic.error "expected type" tok.span);
      Tree.Error
  in
  { Tree.kind; span = tok.span; leading; trailing = [] }

let rec parse_func_decl t =
  let leading = collect_trivia t in
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  let name =
    match (curr t).kind with
    | Token.Ident sym ->
      advance t;
      sym
    | _ ->
      t.diags :=
        Musi_shared.Diagnostic.add
          !(t.diags)
          (Musi_shared.Diagnostic.error "expected identifier" (curr t).span);
      Musi_shared.Interner.intern t.interner "<error>"
  in
  let _ = expect t Token.LParen in
  let params = parse_params t in
  let _ = expect t Token.RParen in
  let ret_typ =
    if (curr t).kind = Token.MinusGt then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let _ = expect t Token.LBrace in
  let body = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let span =
    Musi_shared.Span.make
      (Musi_shared.Span.file start)
      (Musi_shared.Span.start start)
      (Musi_shared.Span.end_ (curr t).span)
  in
  {
    Tree.kind = Tree.Func { name; params; ret_typ; body }
  ; span
  ; leading
  ; trailing = []
  ; sym = None
  }

and parse_params t = parse_separated parse_param Token.Comma Token.RParen t

and parse_param t =
  let leading = collect_trivia t in
  let start = (Token.curr t.stream).span in
  let name =
    match (Token.curr t.stream).kind with
    | Token.Ident sym ->
      Token.advance t.stream;
      sym
    | _ ->
      t.diags :=
        Musi_shared.Diagnostic.add
          !(t.diags)
          (Musi_shared.Diagnostic.error
             "expected parameter name"
             (Token.curr t.stream).span);
      Musi_shared.Interner.intern t.interner "<error>"
  in
  let _ = expect t Token.Colon in
  let ty = parse_ty t in
  let span =
    Musi_shared.Span.make
      (Musi_shared.Span.file start)
      (Musi_shared.Span.start start)
      (Musi_shared.Span.end_ (curr t).span)
  in
  { Tree.name; typ = ty; span; leading; trailing = [] }

let parse_program tokens interner =
  let t = make tokens interner in
  let rec loop acc =
    if (curr t).kind = Token.Eof then List.rev acc
    else
      let decl =
        match (curr t).kind with
        | Token.KwFunc -> parse_func_decl t
        | _ ->
          t.diags :=
            Musi_shared.Diagnostic.add
              !(t.diags)
              (Musi_shared.Diagnostic.error
                 "expected declaration"
                 (curr t).span);
          advance t;
          {
            Tree.kind = Tree.Error
          ; span = (curr t).span
          ; leading = []
          ; trailing = []
          ; sym = None
          }
      in
      loop (decl :: acc)
  in
  (loop [], !(t.diags))
