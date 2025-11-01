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
