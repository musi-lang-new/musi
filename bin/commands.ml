(** CLI command implementations. *)

let format_error_summary diags =
  let error_count = diags.Diagnostic.errors in
  let warning_count = diags.Diagnostic.warnings in
  let error_msg =
    if error_count = 1 then "aborting due to previous error"
    else Printf.sprintf "aborting due to %d previous errors" error_count
  in
  if warning_count > 0 then
    Printf.sprintf
      "%s; %d warning%s emitted"
      error_msg
      warning_count
      (if warning_count = 1 then "" else "s")
  else error_msg

let format_success_summary diags =
  let warning_count = diags.Diagnostic.warnings in
  if warning_count = 0 then "0 errors, 0 warnings"
  else
    Printf.sprintf
      "0 errors, %d warning%s"
      warning_count
      (if warning_count = 1 then "" else "s")

let compile input output_opt =
  let output =
    match output_opt with
    | Some o -> o
    | None -> Filename.remove_extension input ^ ".msc"
  in
  Output.compiling input;
  match Pipeline.compile_file input output with
  | Pipeline.Success _ ->
    Output.finished "dev [fast-compile]" output;
    0
  | Pipeline.Failure diags ->
    let ic = open_in input in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Pipeline.print_diagnostics diags source;
    Output.error (format_error_summary diags);
    1

let check input =
  Output.checking input;
  let ic = open_in input in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let interner = Interner.create () in
  let file_id = 0 in
  let lexer = Lexer.make file_id source interner in
  let tokens, lex_diags = Lexer.lex lexer in
  let ast, parse_diags = Parser.parse_program tokens interner in
  let linker = Linker.create interner [ Sys.getcwd () ] in
  let resolver = Resolver.create_with_linker interner linker in
  let resolve_diags = Resolver.resolve resolver ast in
  let checker = Checker.create interner resolver in
  let check_diags = Checker.check checker ast in

  let all_diags =
    Diagnostic.merge [ lex_diags; parse_diags; resolve_diags; check_diags ]
  in

  if Diagnostic.has_errors all_diags then (
    Pipeline.print_diagnostics all_diags source;
    Output.error (format_error_summary all_diags);
    1)
  else (
    Output.finished "dev [fast-compile]" (format_success_summary all_diags);
    0)

let run input =
  Output.running input;
  let ic = open_in input in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  match Pipeline.compile_source source with
  | Pipeline.Success program ->
    let vm = Vm.create program in
    Vm.run vm
  | Pipeline.Failure diags ->
    Pipeline.print_diagnostics diags source;
    Output.error (format_error_summary diags);
    1

let help () =
  print_endline "Musi";
  print_endline "";
  print_endline "USAGE:";
  print_endline "    musi <COMMAND> [OPTIONS]";
  print_endline "";
  print_endline "COMMANDS:";
  print_endline "    compile <file>    Compile source to bytecode";
  print_endline "    check <file>      Type-check without code generation";
  print_endline "    run <file>        Compile and execute";
  print_endline "    disasm <file>     Disassemble bytecode file";
  print_endline "    help              Print this message";
  print_endline "    version           Print version information";
  print_endline "";
  print_endline "OPTIONS:";
  print_endline "    -o, --output <file>    Specify output file";
  print_endline "    -h, --help             Print help information";
  print_endline "    -V, --version          Print version information";
  0

let disasm input =
  let ic = open_in_bin input in
  let bytes = Bytes.create (in_channel_length ic) in
  really_input ic bytes 0 (Bytes.length bytes);
  close_in ic;
  let program = Bytecode.decode_program bytes in
  Printf.printf "Constants (%d):\n" (Array.length program.Instr.constants);
  Array.iteri
    (fun i const ->
      match const with
      | Instr.CInt32 n -> Printf.printf "  [%d] Int32: %ld\n" i n
      | Instr.CText s -> Printf.printf "  [%d] Text: %S\n" i s)
    program.constants;
  Printf.printf "\nProcedures (%d):\n" (Array.length program.procs);
  Array.iteri
    (fun i proc ->
      Printf.printf
        "\n[%d] %s (params=%d, locals=%d, external=%b):\n"
        i
        (proc : Instr.proc_def).name
        proc.param_count
        proc.local_count
        proc.external_proc;
      if List.length proc.code > 0 then
        List.iteri
          (fun j instr ->
            Printf.printf "  %04d: %s\n" j (Instr.show_instr instr))
          proc.code
      else Printf.printf "  (no code - external or empty)\n")
    program.procs;
  0

let version () =
  print_endline "musi 0.1.0";
  0
