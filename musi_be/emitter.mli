(** Tracks code generation state including locals, constants, and procedures. *)
type t

(** Creates an emitter with access to symbols and string interner. *)
val create : Interner.t -> Resolver.t -> t

(** Translates AST to bytecode instruction sequences. *)
val emit_program : t -> Tree.program -> Instr.program

(** Translates AST and writes bytecode to a file *)
val emit_to_file : t -> Tree.program -> string -> unit
