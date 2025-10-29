type symbol_kind =
  | Bind of { mutable_ : bool; ty : Tree.ty option }
  | Proc of {
        params : Tree.param list
      ; ret_ty : Tree.ty option
    }
  | Type of { fields : Tree.field list option }
  | Extern of { proc_id : int; lib_name : string }

type symbol = {
    name : Interner.symbol
  ; kind : symbol_kind
  ; span : Span.t
}

type t = {
    curr_scope : scope ref
  ; diags : Diagnostic.diagnostic_bag ref
  ; interner : Interner.t
}

and scope

val create : Interner.t -> t
val enter_scope : t -> unit
val exit_scope : t -> unit
val lookup : t -> Interner.symbol -> symbol option
val define : t -> symbol -> unit
