type result =
  | Success of Instr.program
  | Failure of Diagnostic.diagnostic_bag

let compile_string source =
  let interner = Interner.create () in
  let file_id = 0 in
  let lexer = Lexer.make file_id source interner in
  let tokens, lex_diags = Lexer.lex lexer in
  let ast, parse_diags = Parser.parse_program tokens interner in
  let binder = Binder.create interner in
  let bind_diags = Binder.bind_program binder ast in
  let checker = Checker.create interner binder.syms in
  let check_diags = Checker.check_program checker ast in
  let all_diags =
    Diagnostic.merge
      [ lex_diags; parse_diags; bind_diags; check_diags ]
  in
  if Diagnostic.has_errors all_diags then Failure all_diags
  else
    let emitter = Emitter.create interner binder.syms in
    let program = Emitter.emit_program emitter ast in
    Success program

let print_diagnostics diags source =
  let files = Source.empty in
  let _, files = Source.add_file files "<input>" source in
  Diagnostic.emit_all Format.err_formatter diags files

let compile_file input_file output_file =
  let ic = open_in input_file in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  match compile_string source with
  | Success program ->
    let encoded = Bytecode.encode_program program in
    let oc = open_out_bin output_file in
    output_bytes oc encoded;
    close_out oc;
    Success program
  | Failure diags -> Failure diags
