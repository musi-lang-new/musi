type ty =
  | Int
  | Nat
  | Bool
  | Text
  | Unit
  | Proc of { params : ty list; ret : ty }
  | Record of {
        name : Interner.symbol
      ; fields : (Interner.symbol * ty) list
    }
  | Ptr of ty
  | Ref of ty
  | Error

val equal_tys : ty -> ty -> bool
val ty_to_string : Interner.t -> ty -> string
