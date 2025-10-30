(** Garbage collector with mark-and-sweep for cycle detection. *)

(** Register heap object for GC tracking. *)
val register_object : Value.heap_obj -> unit

(** Unregister heap object from GC tracking. *)
val unregister_object : Value.heap_obj -> unit

(** Mark value and transitively mark reachable objects. *)
val mark_value : Value.value -> unit

(** Clear all mark bits before collection cycle. *)
val unmark_all : unit -> unit

(** Free all unmarked heap objects. *)
val sweep_unmarked : unit -> unit

(** Run full GC cycle: unmark, mark roots, sweep unmarked. *)
val collect : Value.value list -> unit
