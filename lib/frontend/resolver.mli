(** Links identifiers to their declarations and detects undefined names. *)

(** Records where a name was declared and what it represents. *)
type symbol = {
    name : Interner.symbol
  ; kind : symbol_kind
  ; span : Span.t
  ; mutable ty : int option
  ; module_path : string option
}

(** Distinguishes variables from procedures. *)
and symbol_kind =
  | SymVar of { mutable_ : bool; weak : bool }
  | SymProc of { params : int; extern_ : Node.abi option }
  | SymAlias of { target : Interner.symbol; target_module : string }

(** Maintains nested scopes and tracks all declared symbols. *)
type t

(** Creates a resolver with an empty root scope. *)
val create : Interner.t -> t

(** Creates a resolver with linker for module resolution. *)
val create_with_linker : Interner.t -> Linker.t -> t

(** Looks up a symbol in the current scope chain. *)
val lookup : t -> Interner.symbol -> symbol option

(** Walks the AST to bind names to declarations, reporting undefined names. *)
val resolve : t -> Node.program -> Diagnostic.diagnostic_bag
