(** Parse token list into AST program *)
val parse_program :
     Token.token list
  -> Musi_shared.Interner.t
  -> Tree.program * Musi_shared.Diagnostic.diagnostic_bag
