(** Runtime value representation with ARC and GC support. *)

type heap_obj = {
    mutable refcount : int
  ; mutable marked : bool
  ; data : obj_data
}

and obj_data = Text of string

type value =
  | Unit
  | Bool of bool
  | Int of int
  | Nat of int
  | HeapRef of heap_obj
