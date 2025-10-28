type result =
  | Success of Musi_codegen.Instr.program
  | Failure of Musi_shared.Diagnostic.diagnostic_bag

val compile_file : string -> string -> result
val compile_string : string -> result
val print_diagnostics : Musi_shared.Diagnostic.diagnostic_bag -> string -> unit
