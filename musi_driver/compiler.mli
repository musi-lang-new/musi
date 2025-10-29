type result =
  | Success of Instr.program
  | Failure of Diagnostic.diagnostic_bag

val compile_file : string -> string -> result
val compile_string : string -> result
val print_diagnostics : Diagnostic.diagnostic_bag -> string -> unit
