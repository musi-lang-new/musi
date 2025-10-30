(** Memory management helpers for heap allocation and ARC. *)

let make_text s =
  let obj = { Value.refcount = 1; marked = false; data = Value.Text s } in
  Gc.register_object obj;
  Value.HeapRef obj

let text_content = function
  | Value.HeapRef { data = Value.Text s; _ } -> s
  | _ -> failwith "not Text value"

let retain = function
  | Value.HeapRef obj -> obj.refcount <- obj.refcount + 1
  | Value.Unit | Value.Bool _ | Value.Int _ | Value.Nat _ -> ()

let release = function
  | Value.HeapRef obj ->
    obj.refcount <- obj.refcount - 1;
    if obj.refcount = 0 then Gc.unregister_object obj
  | Value.Unit | Value.Bool _ | Value.Int _ | Value.Nat _ -> ()
