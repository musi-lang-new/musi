(** Type checker state *)
type t = {
    interner : Musi_shared.Interner.t
  ; syms : Symbol.t
  ; diags : Musi_shared.Diagnostic.diagnostic_bag ref
  ; env : (Musi_shared.Interner.symbol, Types.ty) Hashtbl.t
  ; mutable expected_ret_ty : Types.ty option
}

(** Create type checker with interner and symbol table *)
val create : Musi_shared.Interner.t -> Symbol.t -> t

(** Type-check program and return diagnostics *)
val check_program :
  t -> Musi_syntax.Tree.program -> Musi_shared.Diagnostic.diagnostic_bag
