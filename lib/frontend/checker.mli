(** Infers and validates types using Hindley-Milner with gradual escape hatches. *)

(** Tracks type variables, unification state, and nesting level. *)
type t

(** Creates a type checker with access to resolved symbols. *)
val create : Interner.t -> Resolver.t -> t

(** Walks the AST to infer types and report mismatches. *)
val check : t -> Node.program -> Diagnostic.diagnostic_bag
