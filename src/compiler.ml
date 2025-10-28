type result =
  | Success of Musi_codegen.Instr.program
  | Failure of Musi_shared.Diagnostic.diagnostic_bag

let compile_string source =
  let interner = Musi_shared.Interner.create () in
  let file_id = 0 in
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
  if Musi_shared.Diagnostic.has_errors all_diags then Failure all_diags
  else
    let emitter = Musi_codegen.Emitter.create interner in
    let program = Musi_codegen.Emitter.emit_program emitter ast in
    Success program

let print_diagnostics diags source =
  let files = Musi_shared.Source.empty in
  let _, files = Musi_shared.Source.add_file files "<input>" source in
  Musi_shared.Diagnostic.emit_all Format.err_formatter diags files

let compile_file input_file output_file =
  let ic = open_in input_file in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  match compile_string source with
  | Success program ->
    let encoded = Musi_codegen.Instr.encode_program program in
    let oc = open_out_bin output_file in
    output_bytes oc encoded;
    close_out oc;
    Success program
  | Failure diags -> Failure diags
