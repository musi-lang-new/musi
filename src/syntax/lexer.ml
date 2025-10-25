(* ========================================
  TYPES
  ======================================== *)

type t = {
    file : Musi_shared.Span.file_id
  ; source : string
  ; mutable pos : int
  ; interner : Musi_shared.Interner.t
  ; diags : Musi_shared.Diagnostic.diagnostic_bag ref
}

(* ========================================
  CONSTANTS
  ======================================== *)

let max_unicode_codepoint = 0x10FFFF
let surrogate_start = 0xD800
let surrogate_end = 0xDFFF
let ascii_max = 0x7F
let utf8_min_2byte = 0xC2
let utf8_max_4byte = 0xF4
let hex_base = 16
let unicode_fixed_len = 4
let unicode_long_len = 8
let line_comment_len = 2
let doc_comment_len = 3
let block_comment_len = 2

(* ========================================
  UTILITY FUNCTIONS
  ======================================== *)

let make file source interner =
  {
    file
  ; source
  ; pos = 0
  ; interner
  ; diags = ref Musi_shared.Diagnostic.empty_bag
  }

let at_end t = t.pos >= String.length t.source
let curr_chr t = if at_end t then '\000' else t.source.[t.pos]

let peek_chr t =
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

let consume_while t pred =
  while (not (at_end t)) && pred (curr_chr t) do
    advance t
  done

let matches t str =
  let len = String.length str in
  t.pos + len <= String.length t.source && String.sub t.source t.pos len = str

(* ========================================
  CHARACTER PREDICATES
  ======================================== *)

let is_digit c = c >= '0' && c <= '9'
let is_bdigit c = c = '0' || c = '1'
let is_odigit c = c >= '0' && c <= '7'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_alnum c = is_alpha c || is_digit c
let is_ident_chr c = is_alnum c || c = '_'
let is_number_chr c = is_digit c || c = '_'
let is_suffix_char c = (c >= 'a' && c <= 'z') || is_digit c
let is_whitespace c = c = ' ' || c = '\t' || c = '\r'

let hex_to_int c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
  else if c >= 'A' && c <= 'F' then Char.code c - Char.code 'A' + 10
  else -1

let is_valid_utf8_start c =
  let code = Char.code c in
  code <= ascii_max || (code >= utf8_min_2byte && code <= utf8_max_4byte)

let validate_utf8_char t pos =
  if pos >= String.length t.source then true
  else is_valid_utf8_start t.source.[pos]

(* ========================================
  KEYWORD MAPPING
  ======================================== *)

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
  | "then" -> Some Token.KwThen
  | "trait" -> Some Token.KwTrait
  | "true" -> Some Token.KwTrue
  | "try" -> Some Token.KwTry
  | "unsafe" -> Some Token.KwUnsafe
  | "var" -> Some Token.KwVar
  | "where" -> Some Token.KwWhere
  | "while" -> Some Token.KwWhile
  | "xor" -> Some Token.KwXor
  | _ -> None

(* ========================================
  SYMBOL MAPPING
  ======================================== *)

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

(* ========================================
  UNICODE FUNCTIONS
  ======================================== *)

let validate_unicode_codepoint code pos t =
  if code > max_unicode_codepoint then (
    error t "invalid unicode codepoint out of range" pos;
    false)
  else if code >= surrogate_start && code <= surrogate_end then (
    error t "invalid unicode codepoint in surrogate range" pos;
    false)
  else true

let unicode_to_utf8_char code =
  if code <= ascii_max then Char.chr code else Char.chr (code land ascii_max)

(* ========================================
  SCANNER FUNCTIONS
  ======================================== *)

let scan_ident t start =
  consume_while t is_ident_chr;
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
  if t.pos > start then (
    let suffix_text = slice t start in
    match suffix_of_string suffix_text with
    | Some s -> Some s
    | None ->
      error t ("invalid suffix '" ^ suffix_text ^ "' on numeric literal") start;
      None)
  else if t.pos > 0 && t.source.[t.pos - 1] = '_' then (
    error t "empty numeric suffix" (t.pos - 1);
    None)
  else None

let check_leading_zeros t start =
  if
    t.pos > start + 1 && t.source.[start] = '0' && is_digit t.source.[start + 1]
  then error t "integer literal has leading zeros" start

let scan_binary_number t start =
  advance_n t 2;
  if not (is_bdigit (curr_chr t)) then error t "invalid binary literal" start;
  consume_while t (fun c -> is_bdigit c || c = '_');
  let suffix = scan_suffix t in
  let text = slice t start in
  Token.make (Token.LitInt (text, suffix)) (make_span t start)

let scan_octal_number t start =
  advance_n t 2;
  if not (is_odigit (curr_chr t)) then error t "invalid octal literal" start;
  consume_while t (fun c -> is_odigit c || c = '_');
  let suffix = scan_suffix t in
  let text = slice t start in
  Token.make (Token.LitInt (text, suffix)) (make_span t start)

let scan_hex_number t start =
  advance_n t 2;
  if hex_to_int (curr_chr t) = -1 then error t "invalid hex literal" start;
  consume_while t (fun c -> hex_to_int c >= 0 || c = '_');
  let suffix = scan_suffix t in
  let text = slice t start in
  Token.make (Token.LitInt (text, suffix)) (make_span t start)

let scan_scientific_notation t =
  advance t;
  if curr_chr t = '+' || curr_chr t = '-' then advance t;
  if not (is_digit (curr_chr t)) then
    error t "invalid scientific notation" t.pos
  else consume_while t is_number_chr

let scan_number t start =
  if curr_chr t = '0' then (
    match peek_chr t with
    | 'b' | 'B' -> scan_binary_number t start
    | 'o' | 'O' -> scan_octal_number t start
    | 'x' | 'X' -> scan_hex_number t start
    | _ ->
      consume_while t is_number_chr;
      check_leading_zeros t start;
      let has_dot = curr_chr t = '.' && is_digit (peek_chr t) in
      if has_dot then (
        advance t;
        consume_while t is_number_chr);
      let has_exp = curr_chr t = 'e' || curr_chr t = 'E' in
      if has_exp then scan_scientific_notation t;
      let suffix = scan_suffix t in
      let text = slice t start in
      let kind =
        if has_dot || has_exp then Token.LitFloat (text, suffix)
        else Token.LitInt (text, suffix)
      in
      Token.make kind (make_span t start))
  else (
    consume_while t is_number_chr;
    let has_dot = curr_chr t = '.' && is_digit (peek_chr t) in
    if has_dot then (
      advance t;
      consume_while t is_number_chr);
    let has_exp = curr_chr t = 'e' || curr_chr t = 'E' in
    if has_exp then scan_scientific_notation t;
    let suffix = scan_suffix t in
    let text = slice t start in
    let kind =
      if has_dot || has_exp then Token.LitFloat (text, suffix)
      else Token.LitInt (text, suffix)
    in
    Token.make kind (make_span t start))

let scan_unicode_escape_braced t =
  advance t;
  let start = t.pos in
  consume_while t (fun c -> hex_to_int c >= 0);
  if curr_chr t <> '}' then (
    error t "malformed unicode escape" t.pos;
    '\x00')
  else if t.pos = start then (
    error t "malformed unicode escape" t.pos;
    advance t;
    '\x00')
  else
    let hex_str = slice t start in
    advance t;
    try
      let code = int_of_string ("0x" ^ hex_str) in
      if validate_unicode_codepoint code (t.pos - String.length hex_str - 2) t
      then unicode_to_utf8_char code
      else '\x00'
    with _ ->
      error t "malformed unicode escape" (t.pos - String.length hex_str - 2);
      '\x00'

let scan_unicode_escape_fixed t len =
  let start = t.pos in
  let rec collect_hex acc i =
    if i >= len then acc
    else if at_end t then (
      error t "malformed unicode escape" start;
      acc)
    else
      let digit = hex_to_int (curr_chr t) in
      if digit = -1 then (
        error t "malformed unicode escape" t.pos;
        acc)
      else (
        advance t;
        collect_hex ((acc * hex_base) + digit) (i + 1))
  in
  let code = collect_hex 0 0 in
  if validate_unicode_codepoint code start t then unicode_to_utf8_char code
  else '\x00'

let scan_unicode_escape t =
  if curr_chr t = '{' then scan_unicode_escape_braced t
  else scan_unicode_escape_fixed t unicode_fixed_len

let scan_hex_escape t =
  if at_end t then (
    error t "unterminated escape sequence at end of file" t.pos;
    '\x00')
  else
    let hi = hex_to_int (curr_chr t) in
    if hi = -1 then (
      error t "invalid hex digit in escape sequence" t.pos;
      '\x00')
    else (
      advance t;
      if at_end t then (
        error t "unterminated escape sequence at end of file" t.pos;
        '\x00')
      else
        let lo = hex_to_int (curr_chr t) in
        if lo = -1 then (
          error t "invalid hex digit in escape sequence" t.pos;
          '\x00')
        else (
          advance t;
          Char.chr ((hi * hex_base) + lo)))

let process_escape_char t =
  if at_end t then (
    error t "unterminated escape sequence at end of file" t.pos;
    '\x00')
  else
    let c = curr_chr t in
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
    | 'u' -> scan_unicode_escape t
    | 'U' -> scan_unicode_escape_fixed t unicode_long_len
    | c ->
      error t ("unknown escape sequence '\\" ^ String.make 1 c ^ "'") (t.pos - 1);
      c

let scan_quoted_content t buf quote_char =
  while (not (at_end t)) && curr_chr t <> quote_char && curr_chr t <> '\n' do
    if not (validate_utf8_char t t.pos) then
      error t "invalid UTF-8 sequence" t.pos;
    if curr_chr t = '\\' then (
      advance t;
      Buffer.add_char buf (process_escape_char t))
    else (
      Buffer.add_char buf (curr_chr t);
      advance t)
  done

let scan_text t start =
  advance t;
  let buf = Buffer.create 64 in
  scan_quoted_content t buf '"';
  if curr_chr t <> '"' then error t "unterminated text literal" start
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
      if curr_chr t = '\\' then (
        advance t;
        process_escape_char t)
      else
        let c = curr_chr t in
        advance t;
        c
    in
    if curr_chr t <> '\'' then error t "unterminated rune literal" start
    else advance t;
    Token.make (Token.LitRune (Char.code c)) (make_span t start)

let scan_symbol t start =
  match List.find_opt (fun (sym, _) -> matches t sym) symbols with
  | Some (sym, kind) ->
    advance_n t (String.length sym);
    Token.make kind (make_span t start)
  | None ->
    error t (Printf.sprintf "invalid character '%c'" (curr_chr t)) start;
    advance t;
    Token.make Token.Error (make_span t start)

let scan_whitespace t start =
  consume_while t is_whitespace;
  Token.make Token.Whitespace (make_span t start)

let scan_newline t start =
  advance t;
  Token.make Token.Newline (make_span t start)

let scan_line_comment t start =
  let is_doc =
    t.pos + line_comment_len < String.length t.source
    && t.source.[t.pos + line_comment_len] = '/'
  in
  let skip_len = if is_doc then doc_comment_len else line_comment_len in
  advance_n t skip_len;
  consume_while t (fun ch -> ch <> '\n');
  let text = slice t (start + skip_len) in
  let kind =
    if is_doc then
      Token.LineComment
        { content = Musi_shared.Interner.intern t.interner text }
    else
      Token.LineComment
        { content = Musi_shared.Interner.intern t.interner text }
  in
  Token.make kind (make_span t start)

let scan_block_comment t start =
  advance_n t block_comment_len;
  let docstyle = curr_chr t = '*' && peek_chr t <> '/' in
  let content_start = if docstyle then t.pos + 1 else t.pos in
  if docstyle then advance t;
  let depth = ref 1 in
  while !depth > 0 && not (at_end t) do
    if curr_chr t = '/' && peek_chr t = '*' then (
      advance_n t block_comment_len;
      incr depth)
    else if curr_chr t = '*' && peek_chr t = '/' then (
      advance_n t block_comment_len;
      decr depth)
    else advance t
  done;
  if !depth > 0 then error t "unterminated block comment" start;
  let content_end = if !depth = 0 then t.pos - block_comment_len else t.pos in
  let text =
    if content_end > content_start then
      String.sub t.source content_start (content_end - content_start)
    else ""
  in
  Token.make
    (Token.BlockComment
       { content = Musi_shared.Interner.intern t.interner text; docstyle })
    (make_span t start)

(* ========================================
    MAIN LEXER LOOP
    ======================================== *)

let next_token t =
  let start = t.pos in
  if at_end t then Token.eof (make_span t start)
  else
    match curr_chr t with
    | ' ' | '\t' | '\r' -> scan_whitespace t start
    | '\n' -> scan_newline t start
    | '/' when peek_chr t = '/' -> scan_line_comment t start
    | '/' when peek_chr t = '*' -> scan_block_comment t start
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
