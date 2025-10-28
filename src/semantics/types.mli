type typ =
  | Int
  | Nat
  | Bool
  | Text
  | Unit
  | Proc of { params : typ list; ret : typ }
  | Record of {
        name : Musi_shared.Interner.symbol
      ; fields : (Musi_shared.Interner.symbol * typ) list
    }
  | Error

val equal_typs : typ -> typ -> bool
val typ_to_string : Musi_shared.Interner.t -> typ -> string
