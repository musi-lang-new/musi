let compile input output_opt =
  let output =
    match output_opt with
    | Some o -> o
    | None -> Filename.remove_extension input ^ ".msc"
  in
  Output.compiling input;
  match Musi.Compiler.compile_file input output with
  | Musi.Compiler.Success _ ->
    Output.finished (Printf.sprintf "dev [unoptimized] target -> %s" output);
    0
  | Musi.Compiler.Failure diags ->
    let ic = open_in input in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Musi.Compiler.print_diagnostics diags source;
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
  let interner = Musi_shared.Interner.create () in
  let file_id = 0 in
  let ic = open_in input in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let lexer = Musi_syntax.Lexer.make file_id source interner in
  let tokens, lex_diags = Musi_syntax.Lexer.lex lexer in
  let ast, parse_diags = Musi_syntax.Parser.parse_program tokens interner in
  let binder = Musi_semantics.Binder.create interner in
  let bind_diags = Musi_semantics.Binder.bind_program binder ast in
  let checker = Musi_semantics.Checker.create interner binder.syms in
  let check_diags = Musi_semantics.Checker.check_program checker ast in
  let all_diags =
    Musi_shared.Diagnostic.merge
      [ lex_diags; parse_diags; bind_diags; check_diags ]
  in
  if Musi_shared.Diagnostic.has_errors all_diags then (
    Musi.Compiler.print_diagnostics all_diags source;
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

let run _input =
  Output.error "VM not yet implemented";
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
