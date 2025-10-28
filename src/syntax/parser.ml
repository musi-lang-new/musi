(* ========================================
   TYPES
   ======================================== *)

type t = {
    stream : Token.token_stream
  ; interner : Musi_shared.Interner.t
  ; diags : Musi_shared.Diagnostic.diagnostic_bag ref
}

(* ========================================
   UTILITIES
   ======================================== *)

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

let make_span_from_to start_span end_span =
  Musi_shared.Span.make
    (Musi_shared.Span.file start_span)
    (Musi_shared.Span.start start_span)
    (Musi_shared.Span.end_ end_span)

let make_span_to_curr t start_span = make_span_from_to start_span (curr t).span

(* ========================================
   AST CONSTRUCTORS
   ======================================== *)

let make_expr (kind : Tree.expr_kind) span leading : Tree.expr =
  { Tree.kind; span; leading; trailing = []; ty = None; sym = None }

let make_stmt (kind : Tree.stmt_kind) span leading : Tree.stmt =
  {
    Tree.kind
  ; span
  ; leading
  ; trailing = []
  ; decorators = []
  ; modifiers = Tree.default_modifiers
  ; sym = None
  }

let make_ty (kind : Tree.ty_kind) span leading : Tree.ty =
  { Tree.kind; span; leading; trailing = [] }

let make_decl (kind : Tree.decl_kind) span leading : Tree.decl =
  {
    Tree.kind
  ; span
  ; leading
  ; trailing = []
  ; decorators = []
  ; modifiers = Tree.default_modifiers
  ; sym = None
  }

let make_pat (kind : Tree.pat_kind) span leading : Tree.pat =
  { Tree.kind; span; leading; trailing = []; ty = None }

let error t msg span =
  t.diags :=
    Musi_shared.Diagnostic.add
      !(t.diags)
      (Musi_shared.Diagnostic.error msg span)

let expect t kind =
  if (curr t).kind = kind then (
    advance t;
    true)
  else (
    error t "expected token" (curr t).span;
    false)

(* ========================================
   PRECEDENCE & OPERATORS
   ======================================== *)

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

let prefix_bp = function
  | Token.Minus | Token.KwNot -> Some 13
  | Token.Ampersand -> Some 13
  | _ -> None

let infix_bp = function
  | Token.Dot | Token.LBracket -> Some (15, 16)
  | Token.Caret -> Some (14, 13)
  | Token.Star | Token.Slash | Token.KwMod -> Some (11, 12)
  | Token.Plus | Token.Minus -> Some (9, 10)
  | Token.KwShl | Token.KwShr -> Some (8, 9)
  | Token.DotDotLt | Token.DotDot -> Some (7, 8)
  | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq -> Some (5, 6)
  | Token.Eq | Token.EqSlashEq -> Some (5, 6)
  | Token.KwAnd -> Some (4, 5)
  | Token.KwXor -> Some (3, 4)
  | Token.KwOr -> Some (2, 3)
  | Token.KwAs | Token.KwIs -> Some (1, 2)
  | Token.LtMinus -> Some (0, 1)
  | _ -> None

(* ========================================
   EXPRESSION PARSING
   ======================================== *)

let rec parse_expr t : Tree.expr = parse_expr_bp t 0

and parse_expr_bp t min_bp : Tree.expr =
  let lhs =
    match prefix_bp (curr t).kind with
    | Some bp -> parse_prefix_expr t bp
    | None -> parse_primary_expr t
  in
  parse_infix_expr t lhs min_bp

and parse_ident_expr t sym tok_span leading =
  if (curr t).kind = Token.Dot then (
    advance t;
    if (curr t).kind = Token.LBrace then (
      advance t;
      let fields = parse_record_fields t in
      let _ = expect t Token.RBrace in
      let span = make_span_to_curr t tok_span in
      make_expr (Tree.RecordLiteral { fields }) span leading)
    else (
      error t "expected '{' after '.'" (curr t).span;
      make_expr (Tree.Ident { name = sym }) tok_span leading))
  else make_expr (Tree.Ident { name = sym }) tok_span leading

and parse_primary_expr t : Tree.expr =
  let leading = collect_trivia t in
  let tok = Token.curr t.stream in
  Token.advance t.stream;
  match tok.kind with
  | Token.IntLit (s, suffix) ->
    make_expr (Tree.IntLit { value = s; suffix }) tok.span leading
  | Token.FloatLit (s, suffix) ->
    make_expr (Tree.BinLit { value = s; suffix }) tok.span leading
  | Token.TextLit sym ->
    make_expr (Tree.TextLit { value = sym }) tok.span leading
  | Token.KwTrue -> make_expr (Tree.BoolLit { value = true }) tok.span leading
  | Token.KwFalse -> make_expr (Tree.BoolLit { value = false }) tok.span leading
  | Token.Ident sym -> parse_ident_expr t sym tok.span leading
  | Token.LParen -> parse_paren_or_tuple_expr t tok.span leading
  | Token.LBracket -> parse_array_expr t tok.span leading
  | Token.LBrace -> parse_block_expr t tok.span leading
  | Token.KwConst -> parse_bind_expr t false tok.span leading
  | Token.KwVar -> parse_bind_expr t true tok.span leading
  | Token.KwReturn -> parse_return_expr t tok.span leading
  | Token.KwBreak -> parse_break_expr t tok.span leading
  | Token.KwContinue -> parse_continue_expr tok.span leading
  | Token.KwWhile -> parse_while_expr t tok.span leading
  | Token.KwFor -> parse_for_expr t tok.span leading
  | Token.KwIf -> parse_if_expr t tok.span leading
  | Token.KwMatch -> parse_match_expr t tok.span leading
  | Token.KwTry -> parse_try_expr t tok.span leading
  | Token.KwDefer -> parse_defer_expr t tok.span leading
  | Token.KwAsync -> parse_async_expr t tok.span leading
  | Token.KwAwait -> parse_await_expr t tok.span leading
  | Token.KwProc -> parse_proc_expr t tok.span leading
  | Token.TemplateHead _ -> parse_template_expr t tok.span leading
  | Token.NoSubstTemplateLit _ -> parse_template_expr t tok.span leading
  | _ ->
    error t "expected expression" tok.span;
    make_expr Tree.Error tok.span leading

and parse_prefix_expr t bp : Tree.expr =
  let leading = collect_trivia t in
  let op_tok = Token.curr t.stream in
  Token.advance t.stream;
  let expr = parse_expr_bp t bp in
  let span = make_span_from_to op_tok.span expr.span in
  make_expr (Tree.Unary { op = op_tok.kind; operand = expr }) span leading

and parse_call_args t = parse_separated parse_expr Token.Comma Token.RParen t

and parse_call_expr t (lhs : Tree.expr) =
  (* consume 'LParen' *)
  advance t;
  let args = parse_call_args t in
  let _ = expect t Token.RParen in
  let span = make_span_to_curr t lhs.span in
  (Tree.Call { callee = lhs; args }, span)

and parse_cast_expr t (lhs : Tree.expr) =
  let ty = parse_ty t in
  let span = make_span_from_to lhs.span ty.span in
  (Tree.Cast { expr = lhs; ty }, span)

and parse_test_expr t (lhs : Tree.expr) =
  let ty = parse_ty t in
  let span = make_span_from_to lhs.span ty.span in
  (Tree.Test { expr = lhs; ty }, span)

and parse_field_expr t (lhs : Tree.expr) =
  let field =
    match (curr t).kind with
    | Token.Ident sym ->
      advance t;
      sym
    | Token.IntLit (s, _) ->
      advance t;
      Musi_shared.Interner.intern t.interner s
    | _ ->
      error t "expected field name" (curr t).span;
      Musi_shared.Interner.intern t.interner "<error>"
  in
  let span = make_span_to_curr t lhs.span in
  (Tree.Field { receiver = lhs; field }, span)

and parse_index_expr t (lhs : Tree.expr) =
  let index = parse_expr t in
  let _ = expect t Token.RBracket in
  let span = make_span_to_curr t lhs.span in
  (Tree.Index { receiver = lhs; index }, span)

and parse_assign_expr t (lhs : Tree.expr) =
  let rhs = parse_expr t in
  let span = make_span_from_to lhs.span rhs.span in
  (Tree.Assign { lhs; rhs }, span)

and parse_infix_expr t lhs min_bp =
  match (curr t).kind with
  | Token.LParen ->
    let expr_kind, span = parse_call_expr t lhs in
    parse_infix_expr t (make_expr expr_kind span []) min_bp
  | _ -> (
    match infix_bp (curr t).kind with
    | Some (lbp, rbp) when lbp >= min_bp ->
      let op = (curr t).kind in
      advance t;
      let expr_kind, span =
        match op with
        | Token.KwAs -> parse_cast_expr t lhs
        | Token.KwIs -> parse_test_expr t lhs
        | Token.Dot -> parse_field_expr t lhs
        | Token.LBracket -> parse_index_expr t lhs
        | Token.LtMinus -> parse_assign_expr t lhs
        | _ -> (
          let rhs = parse_expr_bp t rbp in
          let span = make_span_from_to lhs.span rhs.span in
          match op with
          | Token.DotDotLt ->
            (Tree.Range { start = lhs; end_ = rhs; inclusive = false }, span)
          | Token.DotDot ->
            (Tree.Range { start = lhs; end_ = rhs; inclusive = true }, span)
          | _ -> (Tree.Binary { op; lhs; rhs }, span))
      in
      parse_infix_expr t (make_expr expr_kind span []) min_bp
    | _ -> lhs)

and parse_record_fields t =
  parse_separated parse_record_field Token.Comma Token.RBrace t

and parse_record_field t =
  let name =
    match (curr t).kind with
    | Token.Ident sym ->
      advance t;
      sym
    | _ ->
      error t "expected field name" (curr t).span;
      Musi_shared.Interner.intern t.interner "<error>"
  in
  let _ = expect t Token.ColonEq in
  let value = parse_expr t in
  (name, value)

and parse_paren_or_tuple_expr t start leading =
  if (curr t).kind = Token.RParen then (
    advance t;
    let span = make_span_to_curr t start in
    make_expr Tree.UnitLit span leading)
  else if (curr t).kind = Token.Comma then (
    advance t;
    let _ = expect t Token.RParen in
    let span = make_span_to_curr t start in
    make_expr (Tree.Tuple { elems = [] }) span leading)
  else
    let first_expr = parse_expr t in
    if (curr t).kind = Token.Comma then (
      advance t;
      let rest_exprs = parse_separated parse_expr Token.Comma Token.RParen t in
      let _ = expect t Token.RParen in
      let span = make_span_to_curr t start in
      make_expr (Tree.Tuple { elems = first_expr :: rest_exprs }) span leading)
    else
      let _ = expect t Token.RParen in
      first_expr

and parse_block_expr t start leading : Tree.expr =
  let stmts = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let span = make_span_to_curr t start in
  make_expr (Tree.Block { stmts }) span leading

and parse_if_expr t start leading : Tree.expr =
  let cond = parse_expr t in
  let _ = expect t Token.KwThen in
  let _ = expect t Token.LBrace in
  let then_stmts = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let else_br =
    if (curr t).kind = Token.KwElse then (
      advance t;
      let _ = expect t Token.LBrace in
      let stmts = parse_block_stmts t in
      let _ = expect t Token.RBrace in
      Some (make_expr (Tree.Block { stmts }) (make_span_to_curr t start) []))
    else None
  in
  let span = make_span_to_curr t start in
  let then_br = make_expr (Tree.Block { stmts = then_stmts }) span [] in
  make_expr (Tree.If { cond; then_br; else_br }) span leading

and parse_match_expr t start leading : Tree.expr =
  let expr = parse_expr t in
  let _ = expect t Token.LBrace in
  let cases = parse_match_cases t in
  let _ = expect t Token.RBrace in
  let span = make_span_to_curr t start in
  make_expr (Tree.Match { expr; cases }) span leading

and parse_match_cases t =
  let rec loop acc =
    if (curr t).kind = Token.RBrace || (curr t).kind = Token.Eof then
      List.rev acc
    else
      let case = parse_match_case t in
      loop (case :: acc)
  in
  loop []

and parse_match_case t : Tree.match_case =
  let leading = collect_trivia t in
  let start = (curr t).span in
  let _ = expect t Token.KwCase in
  let pat = parse_pat t in
  let guard =
    if (curr t).kind = Token.KwIf then (
      advance t;
      Some (parse_expr t))
    else None
  in
  let _ = expect t Token.MinusGt in
  let body = parse_expr t in
  let _ = if (curr t).kind = Token.Comma then advance t in
  let span = make_span_from_to start body.span in
  { Tree.pat; guard; body; span; leading; trailing = [] }

and parse_array_expr t start leading : Tree.expr =
  let elems = parse_separated parse_expr Token.Comma Token.RBracket t in
  let _ = expect t Token.RBracket in
  let span = make_span_to_curr t start in
  make_expr (Tree.Array { elems }) span leading

and parse_try_expr t start leading : Tree.expr =
  error t "try expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_defer_expr t start leading : Tree.expr =
  let expr = parse_expr t in
  let span = make_span_from_to start expr.span in
  make_expr (Tree.Defer { expr }) span leading

and parse_async_expr t start leading : Tree.expr =
  let expr = parse_expr t in
  let span = make_span_from_to start expr.span in
  make_expr (Tree.Async { expr }) span leading

and parse_await_expr t start leading : Tree.expr =
  let expr = parse_expr t in
  let span = make_span_from_to start expr.span in
  make_expr (Tree.Await { expr }) span leading

and parse_proc_expr t start leading : Tree.expr =
  let _ = expect t Token.LParen in
  let params = parse_params t in
  let _ = expect t Token.RParen in
  let ret_ty =
    if (curr t).kind = Token.MinusGt then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let body =
    if (curr t).kind = Token.LBrace then (
      advance t;
      let stmts = parse_block_stmts t in
      let _ = expect t Token.RBrace in
      Some stmts)
    else None
  in
  let span = make_span_to_curr t start in
  make_expr (Tree.FuncExpr { params; ret_ty; body }) span leading

and parse_template_expr t start leading : Tree.expr =
  error t "template expressions not implemented" start;
  make_expr Tree.Error start leading

(* ========================================
   DECORATOR & MODIFIER PARSING
   ======================================== *)

and parse_decorators t =
  let rec loop acc =
    match (curr t).kind with
    | Token.At ->
      advance t;
      let start = (curr t).span in
      let name =
        match (curr t).kind with
        | Token.Ident sym ->
          advance t;
          sym
        | _ ->
          error t "expected decorator name" (curr t).span;
          Musi_shared.Interner.intern t.interner "<error>"
      in
      let args, span =
        if (curr t).kind = Token.LParen then (
          advance t;
          let args = parse_separated parse_expr Token.Comma Token.RParen t in
          let _ = expect t Token.RParen in
          (args, make_span_to_curr t start))
        else ([], make_span_to_curr t start)
      in
      let decorator = { Tree.name; args; span } in
      loop (decorator :: acc)
    | _ -> List.rev acc
  in
  loop []

and parse_modifiers t =
  let rec loop mods =
    match (curr t).kind with
    | Token.KwExport ->
      advance t;
      loop { mods with Tree.exported = true }
    | Token.KwConst ->
      advance t;
      loop { mods with Tree.constness = true }
    | Token.KwUnsafe ->
      advance t;
      loop { mods with Tree.unsafeness = true }
    | Token.KwAsync ->
      advance t;
      loop { mods with Tree.asyncness = true }
    | Token.KwExtern ->
      advance t;
      let lib_name =
        match (curr t).kind with
        | Token.TextLit sym ->
          advance t;
          Some sym
        | _ -> None
      in
      loop { mods with Tree.externness = (true, lib_name) }
    | _ -> mods
  in
  loop Tree.default_modifiers

(* ========================================
   STATEMENT PARSING
   ======================================== *)

and parse_stmt t : Tree.stmt =
  let leading = collect_trivia t in
  let decorators = parse_decorators t in
  let modifiers = parse_modifiers t in
  let start = (Token.curr t.stream).span in
  let kind =
    match (Token.curr t.stream).kind with
    | Token.KwProc | Token.KwRecord | Token.KwChoice | Token.KwInterface
    | Token.KwAlias ->
      error t "declarations not yet implemented in statement context" start;
      Tree.ExprStmt { expr = make_expr Tree.Error start [] }
    | _ ->
      let expr = parse_expr_bp t 0 in
      Tree.ExprStmt { expr }
  in
  let stmt = make_stmt kind start leading in
  { stmt with decorators; modifiers }

and parse_bind_expr t mutable_ start leading : Tree.expr =
  let pat = parse_pat t in
  let ty =
    if (curr t).kind = Token.Colon then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let _ = expect t Token.ColonEq in
  let init = parse_expr t in
  let span = make_span_from_to start init.span in
  make_expr (Tree.Bind { mutable_; pat; ty; init }) span leading

and parse_return_expr t start leading : Tree.expr =
  let value =
    if (curr t).kind = Token.Semi || (curr t).kind = Token.RBrace then None
    else Some (parse_expr t)
  in
  let span =
    match value with Some e -> make_span_from_to start e.span | None -> start
  in
  make_expr (Tree.Return { value }) span leading

and parse_break_expr t start leading : Tree.expr =
  let value =
    if (curr t).kind = Token.Semi || (curr t).kind = Token.RBrace then None
    else Some (parse_expr t)
  in
  let span =
    match value with Some e -> make_span_from_to start e.span | None -> start
  in
  make_expr (Tree.Break { value }) span leading

and parse_continue_expr start leading : Tree.expr =
  make_expr Tree.Continue start leading

and parse_while_expr t start leading : Tree.expr =
  let cond = parse_expr t in
  let _ = expect t Token.LBrace in
  let body = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let span = make_span_to_curr t start in
  make_expr (Tree.While { cond; body }) span leading

and parse_for_expr t start leading : Tree.expr =
  let pat = parse_pat t in
  let _ = expect t Token.KwIn in
  let iter = parse_expr t in
  let _ = expect t Token.LBrace in
  let body = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let span = make_span_to_curr t start in
  make_expr (Tree.For { pat; iter; body }) span leading

and parse_pat t : Tree.pat =
  let leading = collect_trivia t in
  let tok = Token.curr t.stream in
  Token.advance t.stream;
  let kind : Tree.pat_kind =
    match tok.kind with
    | Token.Ident sym -> Tree.Ident { name = sym }
    | Token.Underscore -> Tree.Wildcard
    | _ ->
      error t "expected pattern" tok.span;
      Tree.Error
  in
  make_pat kind tok.span leading

and parse_block_stmts t =
  let rec loop acc =
    if (curr t).kind = Token.RBrace || (curr t).kind = Token.Eof then
      List.rev acc
    else
      let expr = parse_expr t in
      let stmt = make_stmt (Tree.ExprStmt { expr }) expr.span [] in

      let is_last_expr = (curr t).kind = Token.RBrace in
      if is_last_expr then (
        if (curr t).kind = Token.Semi then advance t;
        loop (stmt :: acc))
      else (
        if (curr t).kind <> Token.Semi then
          error t "expected ';' after statement" (curr t).span;
        advance t;
        loop (stmt :: acc))
  in
  loop []

and parse_ty t : Tree.ty =
  let leading = collect_trivia t in
  let tok = Token.curr t.stream in
  match tok.kind with
  | Token.Star ->
    Token.advance t.stream;
    let inner = parse_ty t in
    let span = make_span_from_to tok.span inner.span in
    make_ty (Tree.Ptr { inner }) span leading
  | Token.Ampersand ->
    Token.advance t.stream;
    let inner = parse_ty t in
    let span = make_span_from_to tok.span inner.span in
    make_ty (Tree.Ref { inner }) span leading
  | Token.Ident sym ->
    Token.advance t.stream;
    make_ty (Tree.Named { name = sym }) tok.span leading
  | _ ->
    Token.advance t.stream;
    error t "expected type" tok.span;
    make_ty Tree.Error tok.span leading

(* ========================================
   DECLARATION PARSING
   ======================================== *)

and parse_import_decl t : Tree.decl =
  let leading = collect_trivia t in
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "import declarations not implemented" start;
  make_decl Tree.Error start leading

and parse_export_decl t : Tree.decl =
  let leading = collect_trivia t in
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "export declarations not implemented" start;
  make_decl Tree.Error start leading

and parse_alias_decl t : Tree.decl =
  let leading = collect_trivia t in
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  let name =
    match (curr t).kind with
    | Token.Ident sym ->
      advance t;
      sym
    | _ ->
      error t "expected identifier" (curr t).span;
      Musi_shared.Interner.intern t.interner "<error>"
  in
  let _ = expect t Token.ColonEq in
  let ty = parse_ty t in
  let _ = expect t Token.Semi in
  let span = make_span_to_curr t start in
  make_decl (Tree.Alias { name; ty }) span leading

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
      error t "expected parameter name" (Token.curr t.stream).span;
      Musi_shared.Interner.intern t.interner "<error>"
  in
  let _ = expect t Token.Colon in
  let ty = parse_ty t in
  let span = make_span_to_curr t start in
  { Tree.name; ty; span; leading; trailing = [] }

(* ========================================
   PUBLIC API
   ======================================== *)

let parse_program tokens interner =
  let t = make tokens interner in
  let rec loop acc =
    if (curr t).kind = Token.Eof then List.rev acc
    else
      let decorators = parse_decorators t in
      let modifiers = parse_modifiers t in
      let decl =
        match (curr t).kind with
        | Token.KwImport -> parse_import_decl t
        | Token.KwExport -> parse_export_decl t
        | Token.KwAlias -> parse_alias_decl t
        | Token.KwProc | Token.KwRecord | Token.KwChoice | Token.KwInterface ->
          error t "declaration not yet implemented" (curr t).span;
          let span = (curr t).span in
          let rec skip_to_end depth =
            match (curr t).kind with
            | Token.Eof -> ()
            | Token.LBrace ->
              advance t;
              skip_to_end (depth + 1)
            | Token.RBrace when depth > 0 ->
              advance t;
              skip_to_end (depth - 1)
            | Token.RBrace -> ()
            | _ ->
              advance t;
              skip_to_end depth
          in
          skip_to_end 0;
          make_decl Tree.Error span []
        | _ ->
          error t "expected declaration" (curr t).span;
          let span = (curr t).span in
          advance t;
          make_decl Tree.Error span []
      in
      loop ({ decl with Tree.decorators; modifiers } :: acc)
  in
  (loop [], !(t.diags))
