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
  | LineComment of { content : Musi_shared.Interner.symbol }
  | BlockComment of { content : Musi_shared.Interner.symbol; docstyle : bool }
  | Error
  | Eof

(** Token with source location *)
type token = { kind : t; span : Musi_shared.Span.t }

(** Token stream for parsing *)
type token_stream

(** Create token with span *)
val make : t -> Musi_shared.Span.t -> token

(** Create EOF token *)
val eof : Musi_shared.Span.t -> token

(** Convert token to string representation *)
val kind_to_string : Musi_shared.Interner.t -> t -> string

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
