(** Builds AST using precedence climbing for expressions. *)

(* ========================================
   PRECEDENCE
   ======================================== *)

type precedence =
  | PrecNone
  | PrecAssign
  | PrecOr
  | PrecXor
  | PrecAnd
  | PrecEquality
  | PrecComparison
  | PrecRange
  | PrecShift
  | PrecTerm
  | PrecFactor
  | PrecPower
  | PrecUnary

let prec_to_int = function
  | PrecNone -> 0
  | PrecAssign -> 1
  | PrecOr -> 2
  | PrecXor -> 3
  | PrecAnd -> 4
  | PrecEquality -> 5
  | PrecComparison -> 6
  | PrecRange -> 7
  | PrecShift -> 8
  | PrecTerm -> 9
  | PrecFactor -> 10
  | PrecPower -> 11
  | PrecUnary -> 12

(* ========================================
   PARSER STATE
   ======================================== *)

type t = {
    stream : Token.token_stream
  ; interner : Interner.t
  ; diags : Diagnostic.diagnostic_bag ref
}

let make tokens interner =
  {
    stream = Token.make_stream tokens
  ; interner
  ; diags = ref Diagnostic.empty_bag
  }

(* ========================================
   UTILITIES
   ======================================== *)

let is_trivia = function
  | Token.Whitespace | Token.Newline | Token.LineComment _
  | Token.BlockComment _ ->
    true
  | _ -> false

let collect_trivia t =
  let rec loop acc =
    let tok = Token.curr t.stream in
    if is_trivia tok.Token.kind then (
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

let error t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg span)

let expect t kind =
  if (curr t).kind = kind then (
    advance t;
    true)
  else (
    error
      t
      (Printf.sprintf "expected %s" (Token.kind_to_string t.interner kind))
      (curr t).span;
    false)

let span_from_to s1 s2 = Span.make (Span.file s1) (Span.start s1) (Span.end_ s2)
let span_to_curr t s = span_from_to s (curr t).span

(* ========================================
   NODE CONSTRUCTORS
   ======================================== *)

let make_node kind span leading =
  {
    Node.kind
  ; span
  ; leading
  ; trailing = []
  ; decorators = []
  ; exported = false
  ; ty = None
  ; sym = None
  }

let make_ty kind span leading = { Node.kind; span; leading; trailing = [] }

(* ========================================
   PRECEDENCE TABLES
   ======================================== *)

let prefix_prec = function
  | Token.Minus | Token.KwNot | Token.Ampersand -> Some PrecUnary
  | _ -> None

let infix_prec = function
  | Token.Caret -> Some (PrecPower, PrecUnary)
  | Token.Star | Token.Slash | Token.KwMod -> Some (PrecFactor, PrecFactor)
  | Token.Plus | Token.Minus -> Some (PrecTerm, PrecTerm)
  | Token.KwShl | Token.KwShr -> Some (PrecShift, PrecShift)
  | Token.DotDotLt | Token.DotDot -> Some (PrecRange, PrecRange)
  | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq ->
    Some (PrecComparison, PrecComparison)
  | Token.Eq | Token.EqSlashEq -> Some (PrecEquality, PrecEquality)
  | Token.KwAnd -> Some (PrecAnd, PrecAnd)
  | Token.KwXor -> Some (PrecXor, PrecXor)
  | Token.KwOr -> Some (PrecOr, PrecOr)
  | Token.KwAs | Token.KwIs -> Some (PrecComparison, PrecComparison)
  | Token.LtMinus -> Some (PrecAssign, PrecAssign)
  | _ -> None

(* ========================================
   EXPRESSION PARSING
   ======================================== *)

let rec parse_expr t = parse_expr_bp t PrecNone

and parse_expr_bp t min_prec =
  let lhs =
    match prefix_prec (curr t).kind with
    | Some prec -> parse_prefix_expr t prec
    | None -> parse_primary_expr t
  in
  let lhs = parse_postfix_expr t lhs in
  parse_infix_expr t lhs min_prec

and parse_primary_expr t =
  let leading = collect_trivia t in
  let tok = curr t in
  advance t;
  let kind =
    match tok.kind with
    | Token.IntLit (s, suffix) -> Node.ExprIntLit { value = s; suffix }
    | Token.FloatLit (s, suffix) -> Node.ExprBinLit { value = s; suffix }
    | Token.TextLit sym -> Node.ExprTextLit { value = sym }
    | Token.KwTrue -> Node.ExprBoolLit { value = true }
    | Token.KwFalse -> Node.ExprBoolLit { value = false }
    | Token.Ident sym -> Node.ExprIdent { name = sym }
    | Token.LParen -> parse_expr_paren_or_tuple t
    | Token.LBracket -> parse_expr_array t
    | Token.LBrace -> parse_expr_block t
    | Token.KwConst -> parse_expr_binding t false
    | Token.KwVar -> parse_expr_binding t true
    | Token.KwReturn -> parse_expr_return t
    | Token.KwBreak -> parse_expr_break t
    | Token.KwContinue -> Node.ExprContinue
    | Token.KwWhile -> parse_expr_while t
    | Token.KwFor -> parse_expr_for t
    | Token.KwIf -> parse_expr_if t
    | Token.KwMatch -> parse_expr_match t
    | Token.KwProc -> parse_expr_proc t
    | Token.KwImport -> parse_expr_import t
    | Token.KwExport -> parse_expr_export t
    | _ ->
      error t "expected expression" tok.span;
      Node.Error
  in
  make_node kind tok.span leading

and parse_prefix_expr t prec =
  let leading = collect_trivia t in
  let op_tok = curr t in
  advance t;
  let operand = parse_expr_bp t prec in
  make_node
    (Node.ExprUnary { op = op_tok.kind; operand })
    (span_from_to op_tok.span operand.span)
    leading

and parse_postfix_expr t lhs =
  match (curr t).kind with
  | Token.LParen ->
    advance t;
    let args = parse_delimited parse_expr Token.Comma Token.RParen t in
    let node =
      make_node
        (Node.ExprCall { callee = lhs; args })
        (span_to_curr t lhs.span)
        []
    in
    parse_postfix_expr t node
  | Token.Dot ->
    advance t;
    let field =
      match (curr t).kind with
      | Token.Ident sym ->
        advance t;
        sym
      | _ ->
        error t "expected field name" (curr t).span;
        Interner.intern t.interner "<error>"
    in
    let node =
      make_node
        (Node.ExprField { receiver = lhs; field })
        (span_to_curr t lhs.span)
        []
    in
    parse_postfix_expr t node
  | Token.LBracket ->
    advance t;
    let index = parse_expr t in
    let _ = expect t Token.RBracket in
    let node =
      make_node
        (Node.ExprIndex { receiver = lhs; index })
        (span_to_curr t lhs.span)
        []
    in
    parse_postfix_expr t node
  | _ -> lhs

and parse_infix_expr t lhs min_prec =
  match infix_prec (curr t).kind with
  | Some (lbp, rbp) when prec_to_int lbp >= prec_to_int min_prec ->
    let op = (curr t).kind in
    advance t;
    let kind, span =
      match op with
      | Token.KwAs ->
        let target = parse_ty t in
        ( Node.ExprCast { inner = lhs; target }
        , span_from_to lhs.span target.span )
      | Token.KwIs ->
        let target = parse_ty t in
        ( Node.ExprTest { inner = lhs; target }
        , span_from_to lhs.span target.span )
      | Token.LtMinus ->
        let rhs = parse_expr_bp t rbp in
        (Node.ExprAssign { lhs; rhs }, span_from_to lhs.span rhs.span)
      | Token.DotDotLt ->
        let end_ = parse_expr_bp t rbp in
        ( Node.ExprRange { start = lhs; end_; inclusive = false }
        , span_from_to lhs.span end_.span )
      | Token.DotDot ->
        let end_ = parse_expr_bp t rbp in
        ( Node.ExprRange { start = lhs; end_; inclusive = true }
        , span_from_to lhs.span end_.span )
      | _ ->
        let rhs = parse_expr_bp t rbp in
        (Node.ExprBinary { op; lhs; rhs }, span_from_to lhs.span rhs.span)
    in
    parse_infix_expr t (make_node kind span []) min_prec
  | _ -> lhs

and parse_expr_paren_or_tuple t =
  if (curr t).kind = Token.RParen then (
    advance t;
    Node.ExprUnitLit)
  else
    let items = parse_delimited parse_expr Token.Comma Token.RParen t in
    if List.length items.items = 1 && List.length items.separators = 0 then
      (List.hd items.items).kind
    else Node.ExprTuple { items }

and parse_expr_array t =
  let items = parse_delimited parse_expr Token.Comma Token.RBracket t in
  Node.ExprArray { items }

and parse_expr_block t =
  let body = parse_separated parse_expr Token.Semi Token.RBrace t in
  let _ = expect t Token.RBrace in
  Node.ExprBlock { body; unsafe_ = false; asyncness = false }

and parse_expr_binding t is_mutable =
  let pat = parse_pat t in
  let ty =
    if (curr t).kind = Token.Colon then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let _ = expect t Token.ColonEq in
  let init = parse_expr t in
  Node.ExprBinding { mutable_ = is_mutable; weakness = false; pat; ty; init }

and parse_expr_return t =
  let value =
    if (curr t).kind = Token.Semi || (curr t).kind = Token.RBrace then None
    else Some (parse_expr t)
  in
  Node.ExprReturn { value }

and parse_expr_break t =
  let value =
    if (curr t).kind = Token.Semi || (curr t).kind = Token.RBrace then None
    else Some (parse_expr t)
  in
  Node.ExprBreak { value }

and parse_expr_while t =
  let cond = parse_expr t in
  let _ = expect t Token.LBrace in
  let body = parse_expr t in
  Node.ExprWhile { cond; body }

and parse_expr_for t =
  let pat = parse_pat t in
  let _ = expect t Token.KwIn in
  let iterable = parse_expr t in
  let _ = expect t Token.LBrace in
  let body = parse_expr t in
  Node.ExprFor { pat; iterable; body }

and parse_expr_if t =
  let cond = parse_expr t in
  let _ = expect t Token.LBrace in
  let then_br = parse_expr t in
  let _ = expect t Token.RBrace in
  let else_br =
    if (curr t).kind = Token.KwElse then (
      advance t;
      let _ = expect t Token.LBrace in
      let br = parse_expr t in
      let _ = expect t Token.RBrace in
      Some br)
    else None
  in
  Node.ExprIf { cond; then_br; else_br }

and parse_expr_match t =
  let scrutinee = parse_expr t in
  let _ = expect t Token.LBrace in
  let rec loop items seps =
    if (curr t).kind = Token.RBrace || (curr t).kind = Token.Eof then
      { Node.items = List.rev items; separators = List.rev seps }
    else
      let item = parse_match_case t in
      if (curr t).kind = Token.Comma then (
        let sep_span = (curr t).span in
        advance t;
        loop (item :: items) (sep_span :: seps))
      else { Node.items = List.rev (item :: items); separators = List.rev seps }
  in
  let cases = loop [] [] in
  let _ = expect t Token.RBrace in
  Node.ExprMatch { scrutinee; cases }

and parse_match_case t : Node.match_case =
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
  {
    Node.pat
  ; guard
  ; body
  ; span = span_from_to start body.span
  ; leading
  ; trailing = []
  }

and parse_expr_import t =
  let _ = expect t Token.LBrace in
  let items = parse_import_export_items t in
  let _ = expect t Token.RBrace in
  let _ = expect t Token.KwFrom in
  let source =
    match (curr t).kind with
    | Token.TextLit sym ->
      advance t;
      sym
    | _ ->
      error t "expected text literal for 'import' source" (curr t).span;
      Interner.intern t.interner "<error>"
  in
  Node.ExprImport { source; kind = Node.Named { items } }

and parse_expr_export t =
  if (curr t).kind = Token.LBrace then (
    advance t;
    let items = parse_import_export_items t in
    let _ = expect t Token.RBrace in
    let source =
      if (curr t).kind = Token.KwFrom then (
        advance t;
        match (curr t).kind with
        | Token.TextLit sym ->
          advance t;
          Some sym
        | _ ->
          error t "expected text literal for 'export' source" (curr t).span;
          Some (Interner.intern t.interner "<error>"))
      else None
    in
    Node.ExprExport { source; kind = Node.Named { items } })
  else (
    error t "expected '{' after 'export'" (curr t).span;
    Node.Error)

and parse_import_export_items t =
  let rec loop acc =
    if (curr t).kind = Token.RBrace || (curr t).kind = Token.Eof then
      List.rev acc
    else
      let start = (curr t).span in
      let name =
        match (curr t).kind with
        | Token.Ident sym ->
          advance t;
          sym
        | _ ->
          error t "expected identifier" (curr t).span;
          Interner.intern t.interner "<error>"
      in
      let alias =
        if (curr t).kind = Token.KwAs then (
          advance t;
          match (curr t).kind with
          | Token.Ident sym ->
            advance t;
            Some sym
          | _ ->
            error t "expected identifier after 'as'" (curr t).span;
            Some (Interner.intern t.interner "<error>"))
        else None
      in
      let item = { Node.name; alias; span = span_to_curr t start } in
      if (curr t).kind = Token.Comma then advance t;
      loop (item :: acc)
  in
  loop []

and parse_expr_proc t =
  let open_span = (curr t).span in
  let _ = expect t Token.LParen in
  let rec loop items seps =
    if (curr t).kind = Token.RParen || (curr t).kind = Token.Eof then
      (List.rev items, List.rev seps)
    else
      let item = parse_param t in
      if (curr t).kind = Token.Comma then (
        let sep_span = (curr t).span in
        advance t;
        loop (item :: items) (sep_span :: seps))
      else (List.rev (item :: items), List.rev seps)
  in
  let items, separators = loop [] [] in
  let close_span = (curr t).span in
  let _ = expect t Token.RParen in
  let params = { Node.open_span; items; separators; close_span } in
  let ret_ty =
    if (curr t).kind = Token.MinusGt then (
      advance t;
      Some (parse_ty t))
    else None
  in
  let body =
    if (curr t).kind = Token.LBrace then (
      advance t;
      Some (parse_expr t))
    else None
  in
  Node.ExprProc
    {
      params
    ; ret_ty
    ; body
    ; asyncness = false
    ; unsafe_ = false
    ; external_ = None
    }

(* ========================================
   PATTERN PARSING
   ======================================== *)

and parse_pat t =
  let leading = collect_trivia t in
  let start = (curr t).span in
  match (curr t).kind with
  | Token.KwConst ->
    advance t;
    let inner = parse_pat t in
    make_node (Node.PatBind { inner }) (span_from_to start inner.span) leading
  | Token.Underscore ->
    advance t;
    make_node Node.PatWildcard start leading
  | Token.Ident sym ->
    advance t;
    make_node (Node.ExprIdent { name = sym }) start leading
  | Token.DotDot ->
    advance t;
    let name =
      match (curr t).kind with
      | Token.Ident sym ->
        advance t;
        Some sym
      | _ -> None
    in
    make_node (Node.PatRest { name }) (span_to_curr t start) leading
  | _ ->
    let expr = parse_expr t in
    make_node (Node.PatExpr { inner = expr }) expr.span leading

(* ========================================
   TYPE PARSING
   ======================================== *)

and parse_ty t =
  let leading = collect_trivia t in
  let tok = curr t in
  match tok.kind with
  | Token.Star ->
    advance t;
    let inner = parse_ty t in
    make_ty (Node.TyPtr { inner }) (span_from_to tok.span inner.span) leading
  | Token.Ampersand ->
    advance t;
    let inner = parse_ty t in
    make_ty (Node.TyRef { inner }) (span_from_to tok.span inner.span) leading
  | Token.Ident sym ->
    advance t;
    make_ty (Node.TyNamed { name = sym }) tok.span leading
  | _ ->
    advance t;
    error t "expected type" tok.span;
    make_ty Node.TyError tok.span leading

and parse_param t =
  let leading = collect_trivia t in
  let start = (curr t).span in
  let name =
    match (curr t).kind with
    | Token.Ident sym ->
      advance t;
      sym
    | _ ->
      error t "expected parameter name" (curr t).span;
      Interner.intern t.interner "<error>"
  in
  let _ = expect t Token.Colon in
  let ty = parse_ty t in
  { Node.name; ty; span = span_to_curr t start; leading; trailing = [] }

(* ========================================
   SEPARATED/DELIMITED PARSING
   ======================================== *)

and parse_separated parse_item sep term t =
  let rec loop items seps =
    if (curr t).kind = term || (curr t).kind = Token.Eof then
      { Node.items = List.rev items; separators = List.rev seps }
    else
      let item = parse_item t in
      if (curr t).kind = sep then (
        let sep_span = (curr t).span in
        advance t;
        loop (item :: items) (sep_span :: seps))
      else { Node.items = List.rev (item :: items); separators = List.rev seps }
  in
  loop [] []

and parse_delimited parse_item sep close t =
  let open_span = (curr t).span in
  let _ =
    expect
      t
      (match close with
      | Token.RParen -> Token.LParen
      | Token.RBracket -> Token.LBracket
      | Token.RBrace -> Token.LBrace
      | _ -> Token.LParen)
  in
  let sep_result = parse_separated parse_item sep close t in
  let close_span = (curr t).span in
  let _ = expect t close in
  {
    Node.open_span
  ; items = sep_result.items
  ; separators = sep_result.separators
  ; close_span
  }

(* ========================================
   TOP-LEVEL PARSING
   ======================================== *)

let parse_program tokens interner =
  let t = make tokens interner in
  let rec loop acc =
    if (curr t).kind = Token.Eof then List.rev acc
    else if (curr t).kind = Token.Semi then (
      advance t;
      loop acc)
    else
      let is_export_stmt =
        if (curr t).kind = Token.KwExport then (
          let saved_pos = t.stream.pos in
          Token.advance t.stream;
          let _ = collect_trivia t in
          let next_is_brace = (Token.curr t.stream).kind = Token.LBrace in
          t.stream.pos <- saved_pos;
          next_is_brace)
        else false
      in
      let exported = (curr t).kind = Token.KwExport && not is_export_stmt in
      if exported then advance t;
      let node = parse_expr t in
      let node =
        if exported then { node with Node.exported = true } else node
      in
      if (curr t).kind = Token.Semi then advance t;
      loop (node :: acc)
  in
  (loop [], !(t.diags))
