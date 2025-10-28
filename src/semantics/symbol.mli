type symbol_kind =
  | Bind of { mutable_ : bool; ty : Musi_syntax.Tree.ty option }
  | Proc of {
        params : Musi_syntax.Tree.param list
      ; ret_ty : Musi_syntax.Tree.ty option
    }
  | Type of { fields : Musi_syntax.Tree.field list option }

type symbol = {
    name : Musi_shared.Interner.symbol
  ; kind : symbol_kind
  ; span : Musi_shared.Span.t
}

type t = {
    curr_scope : scope ref
  ; diags : Musi_shared.Diagnostic.diagnostic_bag ref
  ; interner : Musi_shared.Interner.t
}

and scope

val create : Musi_shared.Interner.t -> t
val enter_scope : t -> unit
val exit_scope : t -> unit
val lookup : t -> Musi_shared.Interner.symbol -> symbol option
val define : t -> symbol -> unit
