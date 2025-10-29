(** Type checker state *)
type t = {
    interner : Interner.t
  ; syms : Symbol.t
  ; diags : Diagnostic.diagnostic_bag ref
  ; env : (Interner.symbol, Types.ty) Hashtbl.t
  ; mutable expected_ret_ty : Types.ty option
}

(** Create type checker with interner and symbol table *)
val create : Interner.t -> Symbol.t -> t

(** Type-check program and return diagnostics *)
val check_program :
  t -> Tree.program -> Diagnostic.diagnostic_bag
