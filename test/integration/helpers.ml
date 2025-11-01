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

let compile_and_run_with_modules modules main_source =
  let dir = Filename.temp_file "musi_test_" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  List.iter
    (fun (name, source) ->
      let path = Filename.concat dir name in
      let oc = open_out path in
      output_string oc source;
      close_out oc)
    modules;
  let interner = Interner.create () in
  let linker = Linker.create interner [ dir ] in
  let lexer = Lexer.make 0 main_source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let resolver = Resolver.create_with_linker interner linker in
  let resolve_diags = Resolver.resolve resolver ast in
  let rec rmdir path =
    if Sys.is_directory path then (
      Sys.readdir path
      |> Array.iter (fun name -> rmdir (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path
  in
  let result =
    if Diagnostic.has_errors resolve_diags then 1
    else
      let emitter = Emitter.create interner resolver in
      let program = Emitter.emit_program emitter ast in
      let vm = Vm.create program in
      Vm.run vm
  in
  rmdir dir;
  result

let compile_and_run_with_module module_name module_source main_source =
  compile_and_run_with_modules [ (module_name, module_source) ] main_source
