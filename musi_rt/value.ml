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

let all_objects = ref []

let make_text s =
  let obj = { refcount = 1; marked = false; data = Text s } in
  all_objects := obj :: !all_objects;
  HeapRef obj

let text_content = function
  | HeapRef { data = Text s; _ } -> s
  | _ -> failwith "not text value"

let retain = function
  | HeapRef obj -> obj.refcount <- obj.refcount + 1
  | Unit | Bool _ | Int _ | Nat _ -> ()

let release = function
  | HeapRef obj ->
    obj.refcount <- obj.refcount - 1;
    if obj.refcount = 0 then
      all_objects := List.filter (fun o -> o != obj) !all_objects
  | Unit | Bool _ | Int _ | Nat _ -> ()

let mark = function
  | HeapRef obj ->
    if not obj.marked then (
      obj.marked <- true;
      match obj.data with Text _ -> ())
  | Unit | Bool _ | Int _ | Nat _ -> ()

let unmark_all () = List.iter (fun obj -> obj.marked <- false) !all_objects

let sweep_unmarked () =
  let to_sweep = List.filter (fun obj -> not obj.marked) !all_objects in
  List.iter (fun obj -> obj.refcount <- 0) to_sweep;
  all_objects := List.filter (fun obj -> obj.marked) !all_objects
