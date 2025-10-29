(** Lexer state *)
type t

(** Create lexer from file ID, source text, and string interner *)
val make : Span.file_id -> string -> Interner.t -> t

(** Tokenize source returning tokens and diagnostics *)
val lex : t -> Token.token list * Diagnostic.diagnostic_bag
