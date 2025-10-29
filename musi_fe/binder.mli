(** Binder state *)
type t = { syms : Symbol.t; interner : Interner.t }

(** Create binder with interner *)
val create : Interner.t -> t

(** Bind program and return diagnostics *)
val bind_program :
  t -> Tree.program -> Diagnostic.diagnostic_bag
