(** Tracks position and context while scanning source text. *)
type t

(** Creates a lexer for a source file. *)
val make : Span.file_id -> string -> Interner.t -> t

(** Scans source text into tokens, reporting any lexical errors. *)
val lex : t -> Token.token list * Diagnostic.diagnostic_bag
