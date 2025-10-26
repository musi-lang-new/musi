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
  { Tree.kind; span; leading; trailing = []; typ = None; sym = None }

let make_stmt (kind : Tree.stmt_kind) span leading : Tree.stmt =
  { Tree.kind; span; leading; trailing = []; sym = None }

let make_typ (kind : Tree.typ_kind) span leading : Tree.typ =
  {
    Tree.kind
  ; span
  ; leading
  ; trailing = []
  ; modifiers = Tree.empty_modifier_set
  }

let make_decl (kind : Tree.decl_kind) span leading modifiers : Tree.decl =
  { Tree.kind; span; leading; trailing = []; sym = None; modifiers }

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
   MODIFIER PARSING
   ======================================== *)

let parse_modifiers t =
  let rec loop modifiers =
    match (curr t).kind with
    | Token.KwExport ->
      advance t;
      loop { modifiers with Tree.exportness = true }
    | Token.KwExtern ->
      advance t;
      let abi =
        match (curr t).kind with
        | Token.LitText sym ->
          advance t;
          Some sym
        | _ -> None
      in
      loop { modifiers with Tree.externness = (true, abi) }
    | Token.KwUnsafe ->
      advance t;
      loop { modifiers with Tree.unsafeness = true }
    | Token.KwConst ->
      advance t;
      loop { modifiers with Tree.constness = true }
    | Token.KwAsync ->
      advance t;
      loop { modifiers with Tree.asyncness = true }
    | _ -> modifiers
  in
  loop Tree.empty_modifier_set

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

let prefix_bp = function Token.Minus | Token.KwNot -> Some 13 | _ -> None

let infix_bp = function
  | Token.Star | Token.Slash -> Some (11, 12)
  | Token.Plus | Token.Minus -> Some (9, 10)
  | Token.DotDotLt | Token.DotDotDot -> Some (7, 8)
  | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.Eq -> Some (5, 6)
  | Token.KwAs | Token.KwIs -> Some (3, 4)
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

and parse_primary_expr t : Tree.expr =
  let leading = collect_trivia t in
  let tok = Token.curr t.stream in
  Token.advance t.stream;
  match tok.kind with
  | Token.LitInt (s, _) ->
    make_expr (Tree.IntLit { value = s }) tok.span leading
  | Token.LitFloat (s, _) ->
    make_expr (Tree.BinLit { value = s }) tok.span leading
  | Token.LitText sym ->
    make_expr (Tree.TextLit { value = sym }) tok.span leading
  | Token.KwTrue -> make_expr (Tree.BoolLit { value = true }) tok.span leading
  | Token.KwFalse -> make_expr (Tree.BoolLit { value = false }) tok.span leading
  | Token.Ident sym -> make_expr (Tree.Ident { name = sym }) tok.span leading
  | Token.LParen -> parse_paren_expr t tok.span leading
  | Token.LBrace -> parse_block_expr t tok.span leading
  | Token.KwIf -> parse_if_expr t tok.span leading
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

and parse_infix_expr t lhs min_bp =
  match (curr t).kind with
  | Token.LParen ->
    advance t;
    let args = parse_call_args t in
    let _ = expect t Token.RParen in
    let span = make_span_to_curr t lhs.span in
    parse_infix_expr
      t
      (make_expr (Tree.Call { callee = lhs; args }) span [])
      min_bp
  | _ -> (
    match infix_bp (curr t).kind with
    | Some (lbp, rbp) when lbp >= min_bp ->
      let op = (curr t).kind in
      advance t;
      let expr_kind, span =
        match op with
        | Token.KwAs ->
          let typ = parse_ty t in
          let span = make_span_from_to lhs.span typ.span in
          (Tree.Cast { expr = lhs; typ }, span)
        | Token.KwIs ->
          let typ = parse_ty t in
          let span = make_span_from_to lhs.span typ.span in
          (Tree.Test { expr = lhs; typ }, span)
        | _ -> (
          let rhs = parse_expr_bp t rbp in
          let span = make_span_from_to lhs.span rhs.span in
          match op with
          | Token.DotDotLt ->
            (Tree.Range { start = lhs; end_ = rhs; inclusive = false }, span)
          | Token.DotDotDot ->
            (Tree.Range { start = lhs; end_ = rhs; inclusive = true }, span)
          | _ -> (Tree.Binary { op; lhs; rhs }, span))
      in
      parse_infix_expr t (make_expr expr_kind span []) min_bp
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
  let span = make_span_to_curr t start in
  make_expr (Tree.Block { stmts }) span leading

and parse_if_expr t start leading : Tree.expr =
  let _ = collect_trivia t in
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

and parse_unit_lit_expr t start leading : Tree.expr =
  error t "unit literal expressions not implemented" start;
  make_expr Tree.UnitLit start leading

and parse_match_expr t start leading : Tree.expr =
  error t "match expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_array_expr t start leading : Tree.expr =
  error t "array expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_tuple_expr t start leading : Tree.expr =
  error t "tuple expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_record_expr t start leading : Tree.expr =
  error t "record expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_field_access_expr t start leading : Tree.expr =
  error t "field access expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_index_access_expr t start leading : Tree.expr =
  error t "index access expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_try_expr t start leading : Tree.expr =
  error t "try expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_defer_expr t start leading : Tree.expr =
  error t "defer expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_async_expr t start leading : Tree.expr =
  error t "async expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_await_expr t start leading : Tree.expr =
  error t "await expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_closure_expr t start leading : Tree.expr =
  error t "closure expressions not implemented" start;
  make_expr Tree.Error start leading

and parse_template_expr t start leading : Tree.expr =
  error t "template expressions not implemented" start;
  make_expr Tree.Error start leading

(* ========================================
   STATEMENT PARSING
   ======================================== *)

and parse_stmt t : Tree.stmt =
  let leading = collect_trivia t in
  match (Token.curr t.stream).kind with
  | Token.KwConst -> parse_bind_stmt t false leading
  | Token.KwVar -> parse_bind_stmt t true leading
  | Token.KwReturn -> parse_return_stmt t leading
  | Token.KwWhile -> parse_while_stmt t leading
  | Token.KwFunc -> parse_decl_stmt t leading
  | Token.KwUnsafe -> parse_unsafe_stmt t leading
  | _ ->
    let expr = parse_expr_bp t 0 in
    if (curr t).kind = Token.LtMinus then parse_assign_stmt t expr leading
    else make_stmt (Tree.Expr { expr }) expr.span leading

and parse_decl_stmt t leading : Tree.stmt =
  let modifiers = parse_modifiers t in
  let decl = parse_func_decl_with_modifiers t modifiers in
  make_stmt (Tree.Decl { decl }) decl.span leading

and parse_unsafe_stmt t leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  let _ = expect t Token.LBrace in
  let stmts = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let span = make_span_to_curr t start in
  make_stmt (Tree.Unsafe { stmts }) span leading

and parse_bind_stmt t mutable_ leading : Tree.stmt =
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
  let typ =
    if (curr t).kind = Token.Colon then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let _ = expect t Token.ColonEq in
  let init = parse_expr t in
  let span = make_span_from_to start init.span in
  make_stmt (Tree.Bind { mutable_; name; typ; init }) span leading

and parse_assign_stmt t lhs leading : Tree.stmt =
  let start = lhs.span in
  advance t;
  let name =
    match lhs.kind with
    | Tree.Ident { name } -> name
    | _ ->
      error t "expression is not assignable" start;
      Musi_shared.Interner.intern t.interner "<error>"
  in
  let rhs = parse_expr t in
  let span = make_span_from_to start rhs.span in
  make_stmt (Tree.Assign { name; value = rhs }) span leading

and parse_return_stmt t leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  let value =
    if (curr t).kind = Token.Semi then None else Some (parse_expr t)
  in
  let _ = expect t Token.Semi in
  let span =
    match value with Some e -> make_span_from_to start e.span | None -> start
  in
  make_stmt (Tree.Return { value }) span leading

and parse_while_stmt t leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  let cond = parse_expr t in
  let _ = expect t Token.LBrace in
  let body = parse_block_stmts t in
  let _ = expect t Token.RBrace in
  let span = make_span_to_curr t start in
  make_stmt (Tree.While { cond; body }) span leading

and parse_break_stmt t leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "break statements not implemented" start;
  make_stmt Tree.Error start leading

and parse_continue_stmt t leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "continue statements not implemented" start;
  make_stmt Tree.Error start leading

and parse_for_stmt t leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "for statements not implemented" start;
  make_stmt Tree.Error start leading

and parse_defer_stmt t leading : Tree.stmt =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "defer statements not implemented" start;
  make_stmt Tree.Error start leading

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
      error t "expected type" tok.span;
      Tree.Error
  in
  make_typ kind tok.span leading

(* ========================================
   DECLARATION PARSING
   ======================================== *)

and parse_func_decl t : Tree.decl =
  parse_func_decl_with_modifiers t Tree.empty_modifier_set

and parse_extern_decl t modifiers : Tree.decl =
  let leading = collect_trivia t in
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  let abi =
    match (curr t).kind with
    | Token.LitText sym ->
      advance t;
      Some sym
    | _ -> None
  in
  let _ = expect t Token.LBrace in
  let decls = parse_extern_block t in
  let _ = expect t Token.RBrace in
  let span = make_span_to_curr t start in
  make_decl (Tree.Extern { abi; decls }) span leading modifiers

and parse_extern_block t =
  let rec loop acc =
    if (curr t).kind = Token.RBrace || (curr t).kind = Token.Eof then
      List.rev acc
    else
      let decl = parse_func_decl t in
      loop (decl :: acc)
  in
  loop []

and parse_record_decl t modifiers : Tree.decl =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "record declarations not implemented" start;
  make_decl Tree.Error start [] modifiers

and parse_choice_decl t modifiers : Tree.decl =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "choice declarations not implemented" start;
  make_decl Tree.Error start [] modifiers

and parse_trait_decl t modifiers : Tree.decl =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "trait declarations not implemented" start;
  make_decl Tree.Error start [] modifiers

and parse_alias_decl t modifiers : Tree.decl =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "alias declarations not implemented" start;
  make_decl Tree.Error start [] modifiers

and parse_import_decl t modifiers : Tree.decl =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "import declarations not implemented" start;
  make_decl Tree.Error start [] modifiers

and parse_export_decl t modifiers : Tree.decl =
  let start = (Token.curr t.stream).span in
  Token.advance t.stream;
  error t "export declarations not implemented" start;
  make_decl Tree.Error start [] modifiers

and parse_func_decl_with_modifiers t modifiers : Tree.decl =
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
  let _ = expect t Token.LParen in
  let params = parse_params t in
  let _ = expect t Token.RParen in
  let ret_typ =
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
    else
      let _ = expect t Token.Semi in
      None
  in
  let span = make_span_to_curr t start in
  make_decl (Tree.Func { name; params; ret_typ; body }) span leading modifiers

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
  { Tree.name; typ = ty; span; leading; trailing = [] }

(* ========================================
   PUBLIC API
   ======================================== *)

let parse_program tokens interner =
  let t = make tokens interner in
  let rec loop acc =
    if (curr t).kind = Token.Eof then List.rev acc
    else
      let modifiers = parse_modifiers t in
      let decl =
        match (curr t).kind with
        | Token.KwFunc -> parse_func_decl_with_modifiers t modifiers
        | Token.KwExtern -> parse_extern_decl t modifiers
        | _ ->
          error t "expected declaration" (curr t).span;
          let span = (curr t).span in
          advance t;
          make_decl Tree.Error span [] Tree.empty_modifier_set
      in
      loop (decl :: acc)
  in
  (loop [], !(t.diags))
