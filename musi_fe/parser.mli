(** Parser state *)
type t

(** Parse token list into AST program *)
val parse_program :
  Token.token list -> Interner.t -> Tree.program * Diagnostic.diagnostic_bag
