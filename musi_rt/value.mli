(** Heap-allocated object with reference count and GC mark bit. *)
type heap_obj

(** Runtime value representation with immediate and heap variants. *)
type value =
  | Unit
  | Bool of bool
  | Int of int
  | Nat of int
  | HeapRef of heap_obj

(** Allocates a text value on the heap. *)
val make_text : string -> value

(** Extracts the string from a text value. *)
val text_content : value -> string

(** Increments reference count for heap values. *)
val retain : value -> unit

(** Decrements reference count and frees if zero. *)
val release : value -> unit

(** Sets the GC mark bit for reachable heap values. *)
val mark : value -> unit

(** Clears all GC mark bits before a collection cycle. *)
val unmark_all : unit -> unit

(** Frees all unmarked heap objects. *)
val sweep_unmarked : unit -> unit
