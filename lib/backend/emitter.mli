(** GIL bytecode emitter with Roslyn-style metadata.

    Two-pass emission architecture:
    1. Collect metadata from bound tree (procedures, strings, types)
    2. Emit bytecode using metadata for lookups

    Bound tree nodes have [node.sym] and [node.ty] filled by resolver and checker. *)

(** Emitter state tracking locals, constants, and metadata. *)
type t

(** Create emitter with access to interner and resolver. *)
val create : Interner.t -> Resolver.t -> t

(** Translate bound tree to GIL bytecode program.

    First pass builds metadata tables, second pass emits instructions.
    Returns program with constant pool and procedure definitions. *)
val emit_program : t -> Node.program -> Instr.program

(** Translate bound tree and write bytecode to file.

    Encodes program as [*.msc] bytecode format. *)
val emit_to_file : t -> Node.program -> string -> unit
