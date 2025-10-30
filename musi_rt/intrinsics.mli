(** Builds runtime error exceptions from messages. *)
type runtime_error_fn = string -> exn

(** Native function callable from bytecode. *)
type intrinsic_fn = Value.value list -> Value.value

(** Adds an intrinsic to a registry. *)
type register_fn = string -> intrinsic_fn -> unit

(** Maps intrinsic names to their native implementations. *)
type t = (string, intrinsic_fn) Hashtbl.t

(** Allocates an empty registry. *)
val create : unit -> t

(** Adds a native function to the registry. *)
val register : t -> string -> intrinsic_fn -> unit

(** Finds a native function by name. *)
val lookup : t -> string -> intrinsic_fn option

(** Builds a registry preloaded with standard library intrinsics. *)
val init_stdlib : runtime_error_fn -> t
