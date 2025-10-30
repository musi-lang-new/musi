(** CLI command implementations. *)

let compile input output_opt =
  let output =
    match output_opt with
    | Some o -> o
    | None -> Filename.remove_extension input ^ ".msc"
  in
  Output.compiling input;
  match Pipeline.compile_file input output with
  | Pipeline.Success _ ->
    Output.finished (Printf.sprintf "dev [unoptimized] target -> %s" output);
    0
  | Pipeline.Failure diags ->
    let ic = open_in input in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Pipeline.print_diagnostics diags source;
    let error_count = diags.errors in
    let msg =
      if error_count = 1 then "could not compile due to previous error"
      else
        Printf.sprintf "could not compile due to %d previous errors" error_count
    in
    Output.error msg;
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
  let resolver = Resolver.create interner in
  let resolve_diags = Resolver.resolve resolver ast in
  let checker = Checker.create interner resolver in
  let check_diags = Checker.check checker ast in

  let all_diags =
    Diagnostic.merge [ lex_diags; parse_diags; resolve_diags; check_diags ]
  in

  if Diagnostic.has_errors all_diags then (
    Pipeline.print_diagnostics all_diags source;
    let error_count = all_diags.errors in
    let msg =
      if error_count = 1 then "could not check due to previous error"
      else
        Printf.sprintf "could not check due to %d previous errors" error_count
    in
    Output.error msg;
    1)
  else (
    Output.finished "type-checking completed";
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
    let error_count = diags.errors in
    let msg =
      if error_count = 1 then "could not run due to previous error"
      else Printf.sprintf "could not run due to %d previous errors" error_count
    in
    Output.error msg;
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
  print_endline "    help              Print this message";
  print_endline "    version           Print version information";
  print_endline "";
  print_endline "OPTIONS:";
  print_endline "    -o, --output <file>    Specify output file";
  print_endline "    -h, --help             Print help information";
  print_endline "    -V, --version          Print version information";
  0

let version () =
  print_endline "musi 0.1.0";
  0
