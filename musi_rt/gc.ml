(** Garbage collector with mark-and-sweep for cycle detection.

    Complements ARC by collecting unreachable cycles. Triggered periodically
    during allocation. *)

let all_objects : Value.heap_obj list ref = ref []
let register_object obj = all_objects := obj :: !all_objects

let unregister_object obj =
  all_objects := List.filter (fun o -> o != obj) !all_objects

let mark_value = function
  | Value.HeapRef obj ->
    if not obj.Value.marked then (
      obj.Value.marked <- true;
      match obj.Value.data with Value.Text _ -> ())
  | Value.Unit | Value.Bool _ | Value.Int _ | Value.Nat _ -> ()

let unmark_all () =
  List.iter (fun obj -> obj.Value.marked <- false) !all_objects

let sweep_unmarked () =
  let to_sweep = List.filter (fun obj -> not obj.Value.marked) !all_objects in
  List.iter (fun obj -> obj.Value.refcount <- 0) to_sweep;
  all_objects := List.filter (fun obj -> obj.Value.marked) !all_objects

let collect roots =
  unmark_all ();
  List.iter mark_value roots;
  sweep_unmarked ()
