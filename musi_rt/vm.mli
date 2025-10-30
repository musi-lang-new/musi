(** Execution state including stack, frames, and instruction pointer. *)
type t

(** Signals runtime failures like type errors or stack overflow. *)
exception Runtime_error of string

(** Initialises a VM with loaded bytecode and stdlib intrinsics. *)
val create : Instr.program -> t

(** Adds a native function callable from bytecode. *)
val register_builtin : t -> string -> (Value.value list -> Value.value) -> unit

(** Runs bytecode from entry point, returning 0 on success or 1 on error. *)
val run : t -> int
