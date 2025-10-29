type heap_obj
type value = Unit | Bool of bool | Int of int | Nat of int | HeapRef of heap_obj

val make_text : string -> value
val text_content : value -> string
val retain : value -> unit
val release : value -> unit
val mark : value -> unit
val unmark_all : unit -> unit
val sweep_unmarked : unit -> unit
