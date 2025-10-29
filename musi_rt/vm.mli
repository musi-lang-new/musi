(** Virtual machine state *)
type t

(** Raised when VM encounters runtime error during execution *)
exception Runtime_error of string

(** Create VM instance from bytecode program *)
val create : Instr.program -> t

(** Register additional builtin function at runtime *)
val register_builtin : t -> string -> (Value.value list -> Value.value) -> unit

(** Execute program starting from entry point, returns exit code *)
val run : t -> int
