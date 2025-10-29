type runtime_error_fn = string -> exn
type intrinsic_fn = Value.value list -> Value.value
type register_fn = string -> intrinsic_fn -> unit
type t = (string, intrinsic_fn) Hashtbl.t

let create () : t = Hashtbl.create 32

let register (reg : t) (name : string) (fn : intrinsic_fn) =
  Hashtbl.add reg name fn

let lookup (reg : t) (name : string) : intrinsic_fn option =
  Hashtbl.find_opt reg name

let init_stdlib runtime_error : t =
  let reg = create () in
  let register_fn = register reg in
  Io.register runtime_error register_fn;
  reg
