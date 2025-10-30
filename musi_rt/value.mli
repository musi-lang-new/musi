(** Runtime value representation with ARC and GC support. *)

(** Heap-allocated object with reference count and GC mark bit. *)
type heap_obj = {
    mutable refcount : int
  ; mutable marked : bool
  ; data : obj_data
}

(** Heap object data variants. *)
and obj_data = Text of string

(** Runtime value with immediate and heap variants. *)
type value =
  | Unit
  | Bool of bool
  | Int of int
  | Nat of int
  | HeapRef of heap_obj
