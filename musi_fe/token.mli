(** Numeric literal suffixes *)
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

(** Token types *)
type t =
  | Ident of Interner.symbol
  | IntLit of string * suffix option
  | FloatLit of string * suffix option
  | TextLit of Interner.symbol
  | RuneLit of int
  | NoSubstTemplateLit of Interner.symbol
  | TemplateHead of Interner.symbol
  | TemplateMiddle of Interner.symbol
  | TemplateTail of Interner.symbol
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
  | KwWeak
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
  | Ampersand
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
  | LineComment of { content : Interner.symbol }
  | BlockComment of { content : Interner.symbol; docstyle : bool }
  | Error
  | Eof

(** Token with source location *)
type token = { kind : t; span : Span.t }

(** Token stream for parsing *)
type token_stream

(** Create token with span *)
val make : t -> Span.t -> token

(** Create EOF token *)
val eof : Span.t -> token

(** Convert token to string representation *)
val kind_to_string : Interner.t -> t -> string

(** Create token stream from list *)
val make_stream : token list -> token_stream

(** Get current token *)
val curr : token_stream -> token

(** Peek next token *)
val peek : token_stream -> token

(** Advance to next token *)
val advance : token_stream -> unit

(** Expect specific token kind *)
val expect : token_stream -> t -> token option
