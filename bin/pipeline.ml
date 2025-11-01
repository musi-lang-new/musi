(** Compilation pipeline orchestrating all compiler stages. *)

type result = Success of Instr.program | Failure of Diagnostic.diagnostic_bag

let compile_source source =
  let interner = Interner.create () in
  let file_id = 0 in

  let lexer = Lexer.make file_id source interner in
  let tokens, lex_diags = Lexer.lex lexer in

  let ast, parse_diags = Parser.parse_program tokens interner in

  let linker = Linker.create interner [ Sys.getcwd () ] in
  let resolver = Resolver.create_with_linker interner linker in

  (* resolve & check each module as it's loaded *)
  let module_diags = ref [] in
  Linker.set_on_module_loaded linker (fun (m : Linker.module_info) ->
    let resolve_diags = Resolver.resolve resolver m.ast in
    let checker = Checker.create interner resolver in
    let check_diags = Checker.check checker m.ast in
    module_diags :=
      Diagnostic.merge [ resolve_diags; check_diags ] :: !module_diags);

  (* resolve & check main AST (triggs module loading) *)
  let resolve_diags_main = Resolver.resolve resolver ast in
  let checker_main = Checker.create interner resolver in
  let check_diags_main = Checker.check checker_main ast in

  let all_diags =
    Diagnostic.merge
      ([ lex_diags; parse_diags; resolve_diags_main; check_diags_main ]
      @ !module_diags)
  in

  if Diagnostic.has_errors all_diags then Failure all_diags
  else
    (* emit combined program for bytecode generation *)
    let modules = Linker.all_modules linker in
    let module_asts =
      List.map (fun (m : Linker.module_info) -> m.ast) modules
    in
    let combined_ast = List.concat (module_asts @ [ ast ]) in
    let emitter = Emitter.create interner resolver in
    let program = Emitter.emit_program emitter combined_ast in
    Success program

let compile_file input_path output_path =
  let ic = open_in input_path in
  let source = really_input_string ic (in_channel_length ic) in
  close_in ic;
  match compile_source source with
  | Success program ->
    let encoded = Bytecode.encode_program program in
    let oc = open_out_bin output_path in
    output_bytes oc encoded;
    close_out oc;
    Success program
  | Failure diags -> Failure diags

let print_diagnostics diags source =
  let files = Source.empty in
  let _, files = Source.add_file files "<input>" source in
  Diagnostic.emit_all Format.err_formatter diags files
