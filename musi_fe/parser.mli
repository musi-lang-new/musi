(** Tracks current position and context during parsing. *)
type t

(** Builds an AST from tokens, reporting syntax errors. *)
val parse_program :
  Token.token list -> Interner.t -> Tree.program * Diagnostic.diagnostic_bag
