(** Outcome of compilation with either bytecode or errors. *)
type result = Success of Instr.program | Failure of Diagnostic.diagnostic_bag

(** Reads a source file, compiles it, and writes bytecode. *)
val compile_file : string -> string -> result

(** Compiles source text through all pipeline stages. *)
val compile_string : string -> result

(** Formats and writes diagnostics with source context to stderr. *)
val print_diagnostics : Diagnostic.diagnostic_bag -> string -> unit
