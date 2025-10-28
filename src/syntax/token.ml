type suffix =
  | I8
  | I16
  | I32
  | I64
  | I128
  | N8
  | N16
  | N32
  | N64
  | N128
  | B16
  | B32
  | B64

type t =
  | Ident of Musi_shared.Interner.symbol
  | IntLit of string * suffix option
  | FloatLit of string * suffix option
  | TextLit of Musi_shared.Interner.symbol
  | RuneLit of int
  | NoSubstTemplateLit of Musi_shared.Interner.symbol
  | TemplateHead of Musi_shared.Interner.symbol
  | TemplateMiddle of Musi_shared.Interner.symbol
  | TemplateTail of Musi_shared.Interner.symbol
  | KwAlias
  | KwAnd
  | KwAs
  | KwAsync
  | KwAwait
  | KwBreak
  | KwCase
  | KwChoice
  | KwConst
  | KwContinue
  | KwDefer
  | KwElse
  | KwExport
  | KwExtern
  | KwFalse
  | KwFor
  | KwFrom
  | KwIf
  | KwImport
  | KwInterface
  | KwIn
  | KwIs
  | KwMatch
  | KwMod
  | KwNot
  | KwOr
  | KwProc
  | KwRecord
  | KwReturn
  | KwSelf
  | KwShl
  | KwShr
  | KwThen
  | KwTrue
  | KwTry
  | KwUnsafe
  | KwVar
  | KwWhere
  | KwWhile
  | KwXor
  | Underscore
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Colon
  | Semi
  | At
  | Question
  | Bang
  | Dollar
  | Plus
  | Minus
  | Star
  | Slash
  | Caret
  | Eq
  | EqSlashEq
  | Lt
  | Gt
  | LtEq
  | GtEq
  | LtMinus
  | MinusGt
  | ColonEq
  | DotDotLt
  | DotDot
  | Whitespace
  | Newline
  | LineComment of { content : Musi_shared.Interner.symbol }
  | BlockComment of { content : Musi_shared.Interner.symbol; docstyle : bool }
  | Error
  | Eof

type token = { kind : t; span : Musi_shared.Span.t }
type token_stream = { tokens : token array; mutable pos : int }

let make kind span = { kind; span }
let eof span = { kind = Eof; span }
let make_stream tokens = { tokens = Array.of_list tokens; pos = 0 }
let at_end s = s.pos >= Array.length s.tokens
let curr s = if at_end s then eof Musi_shared.Span.dummy else s.tokens.(s.pos)

let peek s =
  if s.pos + 1 >= Array.length s.tokens then eof Musi_shared.Span.dummy
  else s.tokens.(s.pos + 1)

let advance s = if not (at_end s) then s.pos <- s.pos + 1

let expect s kind =
  let tok = curr s in
  if tok.kind = kind then (
    advance s;
    Some tok)
  else None

let kind_to_string interner = function
  | Ident sym -> Musi_shared.Interner.to_string interner sym
  | IntLit (s, _) -> s
  | FloatLit (s, _) -> s
  | TextLit sym ->
    Printf.sprintf "\"%s\"" (Musi_shared.Interner.to_string interner sym)
  | RuneLit c -> Printf.sprintf "'%c'" (Char.chr c)
  | NoSubstTemplateLit sym ->
    Printf.sprintf "`%s`" (Musi_shared.Interner.to_string interner sym)
  | TemplateHead sym ->
    Printf.sprintf "`%s${" (Musi_shared.Interner.to_string interner sym)
  | TemplateMiddle sym ->
    Printf.sprintf "}%s${" (Musi_shared.Interner.to_string interner sym)
  | TemplateTail sym ->
    Printf.sprintf "}%s`" (Musi_shared.Interner.to_string interner sym)
  | KwAlias -> "alias"
  | KwAnd -> "and"
  | KwAs -> "as"
  | KwAsync -> "async"
  | KwAwait -> "await"
  | KwBreak -> "break"
  | KwCase -> "case"
  | KwChoice -> "choice"
  | KwConst -> "const"
  | KwContinue -> "continue"
  | KwDefer -> "defer"
  | KwElse -> "else"
  | KwExport -> "export"
  | KwExtern -> "extern"
  | KwFalse -> "false"
  | KwFor -> "for"
  | KwFrom -> "from"
  | KwIf -> "if"
  | KwImport -> "import"
  | KwInterface -> "interface"
  | KwIn -> "in"
  | KwIs -> "is"
  | KwMatch -> "match"
  | KwMod -> "mod"
  | KwNot -> "not"
  | KwOr -> "or"
  | KwProc -> "proc"
  | KwRecord -> "record"
  | KwReturn -> "return"
  | KwSelf -> "self"
  | KwShl -> "shl"
  | KwShr -> "shr"
  | KwThen -> "then"
  | KwTrue -> "true"
  | KwTry -> "try"
  | KwUnsafe -> "unsafe"
  | KwVar -> "var"
  | KwWhere -> "where"
  | KwWhile -> "while"
  | KwXor -> "xor"
  | Underscore -> "_"
  | LParen -> "("
  | RParen -> ")"
  | LBracket -> "["
  | RBracket -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Comma -> ","
  | Dot -> "."
  | Colon -> ":"
  | Semi -> ";"
  | At -> "@"
  | Question -> "?"
  | Bang -> "!"
  | Dollar -> "$"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Caret -> "^"
  | Eq -> "="
  | EqSlashEq -> "=/="
  | Lt -> "<"
  | Gt -> ">"
  | LtEq -> "<="
  | GtEq -> ">="
  | LtMinus -> "<-"
  | MinusGt -> "->"
  | ColonEq -> ":="
  | DotDotLt -> "..<"
  | DotDot -> "..."
  | Whitespace -> "whitespace"
  | Newline -> "newline"
  | LineComment { content } ->
    Printf.sprintf "// %s" (Musi_shared.Interner.to_string interner content)
  | BlockComment { content; _ } ->
    Printf.sprintf "/* %s */" (Musi_shared.Interner.to_string interner content)
  | Error -> "<error>"
  | Eof -> "<eof>"
