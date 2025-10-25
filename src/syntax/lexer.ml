type t = {
    file : Musi_shared.Span.file_id
  ; source : string
  ; mutable pos : int
  ; interner : Musi_shared.Interner.t
  ; diags : Musi_shared.Diagnostic.diagnostic_bag ref
}

let make file source interner =
  {
    file
  ; source
  ; pos = 0
  ; interner
  ; diags = ref Musi_shared.Diagnostic.empty_bag
  }

let at_end t = t.pos >= String.length t.source
let curr_char t = if at_end t then '\000' else t.source.[t.pos]

let peek_char t =
  if t.pos + 1 >= String.length t.source then '\000' else t.source.[t.pos + 1]

let advance t = if not (at_end t) then t.pos <- t.pos + 1

let advance_n t n =
  for _ = 1 to n do
    advance t
  done

let slice t start = String.sub t.source start (t.pos - start)
let make_span t start = Musi_shared.Span.make t.file start t.pos

let error t msg start =
  t.diags :=
    Musi_shared.Diagnostic.add
      !(t.diags)
      (Musi_shared.Diagnostic.error msg (make_span t start))

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_alnum c = is_alpha c || is_digit c
let is_ident_char c = is_alnum c || c = '_'
let is_number_char c = is_digit c || c = '_'
let is_suffix_char c = (c >= 'a' && c <= 'z') || is_digit c
let is_whitespace c = c = ' ' || c = '\t' || c = '\r'

let consume_while t pred =
  while (not (at_end t)) && pred (curr_char t) do
    advance t
  done

let matches t str =
  let len = String.length str in
  t.pos + len <= String.length t.source && String.sub t.source t.pos len = str

let keyword_of_string = function
  | "alias" -> Some Token.KwAlias
  | "and" -> Some Token.KwAnd
  | "as" -> Some Token.KwAs
  | "async" -> Some Token.KwAsync
  | "await" -> Some Token.KwAwait
  | "break" -> Some Token.KwBreak
  | "case" -> Some Token.KwCase
  | "choice" -> Some Token.KwChoice
  | "const" -> Some Token.KwConst
  | "continue" -> Some Token.KwContinue
  | "defer" -> Some Token.KwDefer
  | "else" -> Some Token.KwElse
  | "export" -> Some Token.KwExport
  | "extern" -> Some Token.KwExtern
  | "false" -> Some Token.KwFalse
  | "for" -> Some Token.KwFor
  | "from" -> Some Token.KwFrom
  | "func" -> Some Token.KwFunc
  | "if" -> Some Token.KwIf
  | "import" -> Some Token.KwImport
  | "in" -> Some Token.KwIn
  | "is" -> Some Token.KwIs
  | "let" -> Some Token.KwLet
  | "match" -> Some Token.KwMatch
  | "mod" -> Some Token.KwMod
  | "not" -> Some Token.KwNot
  | "or" -> Some Token.KwOr
  | "record" -> Some Token.KwRecord
  | "return" -> Some Token.KwReturn
  | "self" -> Some Token.KwSelf
  | "shl" -> Some Token.KwShl
  | "shr" -> Some Token.KwShr
  | "trait" -> Some Token.KwTrait
  | "true" -> Some Token.KwTrue
  | "try" -> Some Token.KwTry
  | "unsafe" -> Some Token.KwUnsafe
  | "var" -> Some Token.KwVar
  | "where" -> Some Token.KwWhere
  | "while" -> Some Token.KwWhile
  | "xor" -> Some Token.KwXor
  | _ -> None

let symbols =
  [
    ("=/=", Token.EqSlashEq)
  ; ("..<", Token.DotDotLt)
  ; ("...", Token.DotDotDot)
  ; ("<=", Token.LtEq)
  ; (">=", Token.GtEq)
  ; ("<-", Token.LtMinus)
  ; ("->", Token.MinusGt)
  ; (":=", Token.ColonEq)
  ; ("+", Token.Plus)
  ; ("-", Token.Minus)
  ; ("*", Token.Star)
  ; ("/", Token.Slash)
  ; ("^", Token.Caret)
  ; ("=", Token.Eq)
  ; ("<", Token.Lt)
  ; (">", Token.Gt)
  ; ("(", Token.LParen)
  ; (")", Token.RParen)
  ; ("[", Token.LBracket)
  ; ("]", Token.RBracket)
  ; ("{", Token.LBrace)
  ; ("}", Token.RBrace)
  ; (",", Token.Comma)
  ; (".", Token.Dot)
  ; (":", Token.Colon)
  ; (";", Token.Semi)
  ; ("@", Token.At)
  ; ("?", Token.Question)
  ; ("!", Token.Bang)
  ; ("`", Token.Backtick)
  ; ("$", Token.Dollar)
  ]

let scan_ident t start =
  consume_while t is_ident_char;
  let text = slice t start in
  match keyword_of_string text with
  | Some kw -> Token.make kw (make_span t start)
  | None ->
    Token.make
      (Token.Ident (Musi_shared.Interner.intern t.interner text))
      (make_span t start)

let suffix_of_string = function
  | "i8" -> Some Token.I8
  | "i16" -> Some Token.I16
  | "i32" -> Some Token.I32
  | "i64" -> Some Token.I64
  | "i128" -> Some Token.I128
  | "n8" -> Some Token.N8
  | "n16" -> Some Token.N16
  | "n32" -> Some Token.N32
  | "n64" -> Some Token.N64
  | "n128" -> Some Token.N128
  | "b16" -> Some Token.B16
  | "b32" -> Some Token.B32
  | "b64" -> Some Token.B64
  | _ -> None

let scan_suffix t =
  let start = t.pos in
  consume_while t is_suffix_char;
  if t.pos > start then suffix_of_string (slice t start) else None

let scan_number t start =
  consume_while t is_number_char;
  let has_dot = curr_char t = '.' && is_digit (peek_char t) in
  if has_dot then (
    advance t;
    consume_while t is_number_char);
  let suffix = scan_suffix t in
  let text = slice t start in
  let kind =
    if has_dot then Token.LitFloat (text, suffix)
    else Token.LitInt (text, suffix)
  in
  Token.make kind (make_span t start)

let hex_to_int c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
  else if c >= 'A' && c <= 'F' then Char.code c - Char.code 'A' + 10
  else -1

let scan_hex_escape t =
  if at_end t then (
    error t "invalid hex escape sequence" t.pos;
    '\x00')
  else
    let hi = hex_to_int (curr_char t) in
    if hi = -1 then (
      error t "invalid hex escape sequence" t.pos;
      '\x00')
    else (
      advance t;
      if at_end t then Char.chr hi
      else
        let lo = hex_to_int (curr_char t) in
        if lo = -1 then Char.chr hi
        else (
          advance t;
          Char.chr ((hi * 16) + lo)))

let process_escape_char t =
  if at_end t then (
    error t "unexpected end of file in escape sequence" t.pos;
    '\x00')
  else
    let c = curr_char t in
    advance t;
    match c with
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | '\\' -> '\\'
    | '"' -> '"'
    | '\'' -> '\''
    | '`' -> '`'
    | '0' -> '\x00'
    | 'x' -> scan_hex_escape t
    | c ->
      error t ("unknown escape sequence '\\" ^ String.make 1 c ^ "'") (t.pos - 1);
      c

let scan_quoted_content t buf quote_char =
  while (not (at_end t)) && curr_char t <> quote_char && curr_char t <> '\n' do
    if curr_char t = '\\' then (
      advance t;
      Buffer.add_char buf (process_escape_char t))
    else (
      Buffer.add_char buf (curr_char t);
      advance t)
  done

let scan_text t start =
  advance t;
  let buf = Buffer.create 64 in
  scan_quoted_content t buf '"';
  if curr_char t <> '"' then error t "unterminated text literal" start
  else advance t;
  let text = Buffer.contents buf in
  Token.make
    (Token.LitText (Musi_shared.Interner.intern t.interner text))
    (make_span t start)

let scan_rune t start =
  advance t;
  if at_end t then (
    error t "empty rune literal" start;
    Token.make Token.Error (make_span t start))
  else
    let c =
      if curr_char t = '\\' then (
        advance t;
        process_escape_char t)
      else
        let c = curr_char t in
        advance t;
        c
    in
    if curr_char t <> '\'' then error t "unterminated rune literal" start
    else advance t;
    Token.make (Token.LitRune (Char.code c)) (make_span t start)

let scan_symbol t start =
  match List.find_opt (fun (sym, _) -> matches t sym) symbols with
  | Some (sym, kind) ->
    advance_n t (String.length sym);
    Token.make kind (make_span t start)
  | None ->
    error t (Printf.sprintf "invalid character '%c'" (curr_char t)) start;
    advance t;
    Token.make Token.Error (make_span t start)

let scan_whitespace t start =
  consume_while t is_whitespace;
  Token.make Token.Whitespace (make_span t start)

let scan_newline t start =
  advance t;
  Token.make Token.Newline (make_span t start)

let scan_line_comment t start =
  advance_n t 2;
  consume_while t (fun ch -> ch <> '\n');
  let text = slice t (start + 2) in
  Token.make
    (Token.LineComment { content = Musi_shared.Interner.intern t.interner text })
    (make_span t start)

let scan_block_comment t start =
  advance_n t 2;
  let docstyle = curr_char t = '*' && peek_char t <> '/' in
  let content_start = t.pos in
  let depth = ref 1 in
  while !depth > 0 && not (at_end t) do
    if curr_char t = '/' && peek_char t = '*' then (
      advance_n t 2;
      incr depth)
    else if curr_char t = '*' && peek_char t = '/' then (
      advance_n t 2;
      decr depth)
    else advance t
  done;
  if !depth > 0 then error t "unterminated block comment" start;
  let content_end = if !depth = 0 then t.pos - 2 else t.pos in
  let text =
    if content_end > content_start then
      String.sub t.source content_start (content_end - content_start)
    else ""
  in
  Token.make
    (Token.BlockComment
       { content = Musi_shared.Interner.intern t.interner text; docstyle })
    (make_span t start)

let next_token t =
  let start = t.pos in
  if at_end t then Token.eof (make_span t start)
  else
    match curr_char t with
    | ' ' | '\t' | '\r' -> scan_whitespace t start
    | '\n' -> scan_newline t start
    | '/' when peek_char t = '/' -> scan_line_comment t start
    | '/' when peek_char t = '*' -> scan_block_comment t start
    | '"' -> scan_text t start
    | '\'' -> scan_rune t start
    | '0' .. '9' -> scan_number t start
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> scan_ident t start
    | _ -> scan_symbol t start

let lex t =
  let rec loop acc =
    let tok = next_token t in
    if tok.kind = Token.Eof then List.rev (tok :: acc) else loop (tok :: acc)
  in
  let tokens = loop [] in
  let final_diags = !(t.diags) in
  (tokens, final_diags)
