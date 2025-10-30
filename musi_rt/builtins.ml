(** Native functions callable from bytecode. *)

type builtin_fn = Value.value list -> Value.value
type t = (string, builtin_fn) Hashtbl.t

let create () = Hashtbl.create 16
let register t name fn = Hashtbl.add t name fn
let lookup t name = Hashtbl.find_opt t name

let prep_stdlib () =
  let t = create () in

  register t "__builtin_writeln" (fun args ->
    match args with
    | [ Value.HeapRef { data = Value.Text s; _ } ] ->
      print_endline s;
      Value.Unit
    | _ -> failwith "procedure 'writeln' expects 'Text' argument");
  register t "__builtin_write" (fun args ->
    match args with
    | [ Value.HeapRef { data = Value.Text s; _ } ] ->
      print_string s;
      Value.Unit
    | _ -> failwith "procedure 'write' expects 'Text' argument");

  t
