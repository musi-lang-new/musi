(** Type checker with bidirectional HM and gradual typing. *)

(** Type checker state. *)
type t

(** Create new type checker. *)
val create : Interner.t -> Resolver.t -> t

(** Type check program, returning diagnostics. *)
val check : t -> Tree.program -> Diagnostic.diagnostic_bag
