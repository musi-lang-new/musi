val encode_instr : Instr.instr -> bytes
val encode_constant : Instr.constant -> bytes
val encode_proc : Instr.proc_def -> bytes
val encode_procs : Instr.proc_def array -> bytes
val encode_record_type : Instr.record_type_def -> bytes
val encode_record_types : Instr.record_type_def list -> bytes
val encode_constants : Instr.constant array -> bytes
val encode_program : Instr.program -> bytes
