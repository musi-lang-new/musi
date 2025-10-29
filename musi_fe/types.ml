(** Type representation for semantic analysis. *)

type ty_repr =
  | Int
  | Nat
  | Bool
  | Text
  | Unit
  | Proc of { params : ty_repr list; ret : ty_repr }
  | Record of {
        name : Interner.symbol
      ; fields : (Interner.symbol * ty_repr) list
    }
  | Ptr of ty_repr
  | Ref of ty_repr
  | Error

let rec equal_types t1 t2 =
  match (t1, t2) with
  | Int, Int | Nat, Nat | Bool, Bool | Text, Text | Unit, Unit | Error, Error ->
    true
  | Proc { params = p1; ret = r1 }, Proc { params = p2; ret = r2 } ->
    List.length p1 = List.length p2
    && List.for_all2 equal_types p1 p2
    && equal_types r1 r2
  | Record { name = n1; _ }, Record { name = n2; _ } -> n1 = n2
  | Ptr t1, Ptr t2 -> equal_types t1 t2
  | Ref t1, Ref t2 -> equal_types t1 t2
  | _ -> false

let rec type_to_string interner = function
  | Int -> "Int"
  | Nat -> "Nat"
  | Bool -> "Bool"
  | Text -> "Text"
  | Unit -> "Unit"
  | Proc { params; ret } ->
    let params_str =
      String.concat ", " (List.map (type_to_string interner) params)
    in
    Printf.sprintf "proc(%s) -> %s" params_str (type_to_string interner ret)
  | Record { name; _ } -> Interner.to_string interner name
  | Ptr t -> Printf.sprintf "*%s" (type_to_string interner t)
  | Ref t -> Printf.sprintf "&%s" (type_to_string interner t)
  | Error -> "<error>"
