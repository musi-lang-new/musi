(** Links identifiers to their declarations and detects undefined names. *)

(** Records where a name was declared and what it represents. *)
type symbol = {
    name : Interner.symbol
  ; kind : symbol_kind
  ; span : Span.t
  ; mutable ty : int option  (** Type checker fills this later *)
}

(** Distinguishes variables from procedures. *)
and symbol_kind =
  | SymVar of { mutable_ : bool; weak : bool }
  | SymProc of { params : int; extern_ : Tree.abi option }

(** Maintains nested scopes and tracks all declared symbols. *)
type t

(** Initializes a resolver with an empty root scope. *)
val create : Interner.t -> t

(** Walks the AST to bind names to declarations, reporting undefined names. *)
val resolve : t -> Tree.program -> Diagnostic.diagnostic_bag
