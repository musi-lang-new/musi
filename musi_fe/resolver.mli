(** Name resolution and symbol table construction. *)

(** Symbol table entry. *)
type symbol = {
    name : Interner.symbol
  ; kind : symbol_kind
  ; span : Span.t
  ; mutable ty : int option  (** Filled by type checker *)
}

(** Symbol kinds. *)
and symbol_kind =
  | SymVar of { mutable_ : bool; weak : bool }
  | SymProc of { params : int; extern_ : Tree.abi option }

(** Symbol table with scoped resolution. *)
type t

(** Create new resolver. *)
val create : Interner.t -> t

(** Resolve names in program, returning symbol table and diagnostics. *)
val resolve : t -> Tree.program -> Diagnostic.diagnostic_bag
