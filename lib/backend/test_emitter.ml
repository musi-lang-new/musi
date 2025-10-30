open Alcotest

let make_emitter source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let resolver = Resolver.create interner in
  let _resolve_diags = Resolver.resolve resolver ast in
  let emitter = Emitter.create interner resolver in
  let program = Emitter.emit_program emitter ast in
  program

let test_simple_proc () =
  let program = make_emitter "const f := proc (x: Int) -> Int { x };" in
  check int "main plus one procedure" 2 (Array.length program.procs)

let test_proc_call () =
  let program =
    make_emitter "const f := proc (x: Int) -> Int { x }; const y := f(1);"
  in
  check int "main plus one procedure" 2 (Array.length program.procs)

let test_multiple_procs () =
  let program =
    make_emitter
      "const f := proc (x: Int) -> Int { x }; const g := proc (y: Int) -> Int \
       { y };"
  in
  check int "main plus two procedures" 3 (Array.length program.procs)

let test_no_procs () =
  let program = make_emitter "const x := 1;" in
  check int "only main procedure" 1 (Array.length program.procs)

let () =
  run
    "Emitter"
    [
      ( "Procedures"
      , [
          test_case "simple_proc" `Quick test_simple_proc
        ; test_case "proc_call" `Quick test_proc_call
        ; test_case "multiple_procs" `Quick test_multiple_procs
        ; test_case "no_procs" `Quick test_no_procs
        ] )
    ]
