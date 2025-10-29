type t

val create : Interner.t -> Symbol.t -> t
val emit_program : t -> Tree.stmt list -> Instr.program
val emit_to_file : t -> Tree.stmt list -> string -> unit
