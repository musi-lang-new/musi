(** Function that constructs runtime error exceptions *)
type runtime_error_fn = string -> exn

(** Native OCaml function implementing an intrinsic *)
type intrinsic_fn = Value.value list -> Value.value

(** Callback for registering intrinsics by name *)
type register_fn = string -> intrinsic_fn -> unit

(** Registry mapping intrinsic names to implementations *)
type t = (string, intrinsic_fn) Hashtbl.t

(** Create empty intrinsic registry *)
val create : unit -> t

(** Register intrinsic function by name *)
val register : t -> string -> intrinsic_fn -> unit

(** Look up intrinsic by name *)
val lookup : t -> string -> intrinsic_fn option

(** Create registry with all stdlib intrinsics registered *)
val init_stdlib : runtime_error_fn -> t
