(** Compilation result *)
type result = Success of Instr.program | Failure of Diagnostic.diagnostic_bag

(** Compile source file to bytecode file *)
val compile_file : string -> string -> result

(** Compile source string to bytecode program *)
val compile_string : string -> result

(** Print diagnostics to stderr with source context *)
val print_diagnostics : Diagnostic.diagnostic_bag -> string -> unit
