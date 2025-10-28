(** Binder state *)
type t = { syms : Symbol.t; interner : Musi_shared.Interner.t }

(** Create binder with interner *)
val create : Musi_shared.Interner.t -> t

(** Bind program and return diagnostics *)
val bind_program :
  t -> Musi_syntax.Tree.program -> Musi_shared.Diagnostic.diagnostic_bag
