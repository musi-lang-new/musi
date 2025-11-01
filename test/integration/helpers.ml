let compile_and_run source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let resolver = Resolver.create interner in
  let _resolve_diags = Resolver.resolve resolver ast in
  let emitter = Emitter.create interner resolver in
  let program = Emitter.emit_program emitter ast in
  let vm = Vm.create program in
  Vm.run vm

let compile_and_run_with_module module_name module_source main_source =
  let dir = Filename.temp_file "musi_test_" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  let module_path = Filename.concat dir module_name in
  let oc = open_out module_path in
  output_string oc module_source;
  close_out oc;
  let interner = Interner.create () in
  let linker = Linker.create interner [ dir ] in
  let lexer = Lexer.make 0 main_source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let resolver = Resolver.create_with_linker interner linker in
  let _resolve_diags = Resolver.resolve resolver ast in
  let emitter = Emitter.create interner resolver in
  let program = Emitter.emit_program emitter ast in
  let vm = Vm.create program in
  let result = Vm.run vm in
  let rec rmdir path =
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name -> rmdir (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path
  in
  rmdir dir;
  result
