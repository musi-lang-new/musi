(** Compilation pipeline orchestrating all compiler stages. *)

(** Compilation outcome with either bytecode or diagnostics. *)
type result = Success of Instr.program | Failure of Diagnostic.diagnostic_bag

(** Compile source text through full pipeline.

    Stages: Lexer -> Parser -> Resolver -> Checker -> Emitter *)
val compile_source : string -> string list -> result

(** Read source file, compile, and write bytecode to output file. *)
val compile_file : string -> string -> string list -> result

(** Format and write diagnostics with source context to stderr. *)
val print_diagnostics : Diagnostic.diagnostic_bag -> string -> unit
