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
  (* Identifiers *)
  | Ident of Musi_shared.Interner.symbol
  (* Literals *)
  | LitInt of string * suffix option
  | LitFloat of string * suffix option
  | LitText of Musi_shared.Interner.symbol
  | LitRune of int
  | LitNoSubstTemplate of Musi_shared.Interner.symbol
  | TemplateHead of Musi_shared.Interner.symbol
  | TemplateMiddle of Musi_shared.Interner.symbol
  | TemplateTail of Musi_shared.Interner.symbol
  (* Keywords (alphabetically) *)
  | KwAlias
  | KwAnd
  | KwAs
  | KwAsync
  | KwAwait
  | KwBreak
  | KwCase
  | KwChoice
  | KwContinue
  | KwDefer
  | KwElse
  | KwExport
  | KwExtern
  | KwFalse
  | KwFor
  | KwFrom
  | KwFunc
  | KwIf
  | KwImport
  | KwIn
  | KwIs
  | KwLet
  | KwMatch
  | KwMod
  | KwNot
  | KwOr
  | KwRecord
  | KwReturn
  | KwSelf
  | KwShl
  | KwShr
  | KwThen
  | KwTrait
  | KwTrue
  | KwTry
  | KwUnsafe
  | KwVar
  | KwWhere
  | KwWhile
  | KwXor
  (* Delimiters *)
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
  (* Operators *)
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
  | DotDotDot
  (* Trivia *)
  | Whitespace
  | Newline
  | LineComment of { content : Musi_shared.Interner.symbol }
  | BlockComment of { content : Musi_shared.Interner.symbol; docstyle : bool }
  (* Special *)
  | Error
  | Eof

type token = { kind : t; span : Musi_shared.Span.t }

val make : t -> Musi_shared.Span.t -> token
val eof : Musi_shared.Span.t -> token
val kind_to_string : Musi_shared.Interner.t -> t -> string

type token_stream

val make_stream : token list -> token_stream
val curr : token_stream -> token
val peek : token_stream -> token
val advance : token_stream -> unit
val expect : token_stream -> t -> token option
