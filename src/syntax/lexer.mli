type t

val make : Musi_shared.Span.file_id -> string -> Musi_shared.Interner.t -> t
val lex : t -> Token.token list * Musi_shared.Diagnostic.diagnostic_bag
