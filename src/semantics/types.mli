type ty =
  | Int
  | Nat
  | Bool
  | Text
  | Unit
  | Proc of { params : ty list; ret : ty }
  | Record of {
        name : Musi_shared.Interner.symbol
      ; fields : (Musi_shared.Interner.symbol * ty) list
    }
  | Error

val equal_tys : ty -> ty -> bool
val ty_to_string : Musi_shared.Interner.t -> ty -> string
