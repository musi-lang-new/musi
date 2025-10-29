(** Kind of symbol in symbol table *)
type symbol_kind =
  | Bind of { mutable_ : bool; ty : Tree.ty option }
  | Proc of { params : Tree.param list; ret_ty : Tree.ty option }
  | Type of { fields : Tree.field list option }
  | Extern of { proc_id : int; lib_name : string }

(** Symbol table entry *)
type symbol = { name : Interner.symbol; kind : symbol_kind; span : Span.t }

(** Symbol table with scoped name resolution *)
type t = {
    curr_scope : scope ref
  ; diags : Diagnostic.diagnostic_bag ref
  ; interner : Interner.t
}

and scope

(** Create new symbol table *)
val create : Interner.t -> t

(** Enter new lexical scope *)
val enter_scope : t -> unit

(** Exit current scope, returning to parent *)
val exit_scope : t -> unit

(** Look up symbol by name in current scope chain *)
val lookup : t -> Interner.symbol -> symbol option

(** Define symbol in current scope *)
val define : t -> symbol -> unit

(** Iterate over all symbols in scope chain *)
val iter_all : t -> (symbol -> unit) -> unit
