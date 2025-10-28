type t

val create : Musi_shared.Interner.t -> t
val emit_program : t -> Musi_syntax.Tree.stmt list -> Instr.program
val emit_to_file : t -> Musi_syntax.Tree.stmt list -> string -> unit
