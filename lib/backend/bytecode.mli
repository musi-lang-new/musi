(** Serializes an instruction to bytecode format. *)
val encode_instr : Instr.instr -> bytes

(** Serializes a constant value with type tag. *)
val encode_constant : Instr.constant -> bytes

(** Serializes a procedure definition with metadata and code. *)
val encode_proc : Instr.proc_def -> bytes

(** Serializes all procedures with count prefix. *)
val encode_procs : Instr.proc_def array -> bytes

(** Serializes a record type definition with field metadata. *)
val encode_record_type : Instr.record_type_def -> bytes

(** Serializes all record types with count prefix. *)
val encode_record_types : Instr.record_type_def list -> bytes

(** Serializes constant pool with count prefix. *)
val encode_constants : Instr.constant array -> bytes

(** Serializes a complete program with header and all sections. *)
val encode_program : Instr.program -> bytes
