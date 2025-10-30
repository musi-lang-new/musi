(** Native functions callable from bytecode. *)

(** Native function signature taking value list and returning value. *)
type builtin_fn = Value.value list -> Value.value

(** Builtin function registry. *)
type t

(** Create empty builtin registry. *)
val create : unit -> t

(** Register native function by name. *)
val register : t -> string -> builtin_fn -> unit

(** Look up native function by name. *)
val lookup : t -> string -> builtin_fn option

(** Prepare registry with standard library builtins. *)
val prep_stdlib : unit -> t
