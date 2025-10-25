(** Lexer state *)
type t

(** Create lexer from file ID, source text, and string interner *)
val make : Musi_shared.Span.file_id -> string -> Musi_shared.Interner.t -> t

(** Tokenize source returning tokens and diagnostics *)
val lex : t -> Token.token list * Musi_shared.Diagnostic.diagnostic_bag
