open Alcotest
module Lexer = Lexer
module Parser = Parser
module Emitter = Emitter
module Interner = Interner

let make_emitter source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let binder = Binder.create interner in
  let _bind_diags = Binder.bind_program binder ast in
  let emitter = Emitter.create interner binder.syms in
  let program = Emitter.emit_program emitter ast in
  program

let test_simple_proc () =
  let program = make_emitter "const f := proc (x: Int) -> Int { x };" in
  check int "one procedure emitted" 4 (Array.length program.procs)

let test_proc_call () =
  let program =
    make_emitter "const f := proc (x: Int) -> Int { x }; const y := f(1);"
  in
  check int "one procedure emitted" 4 (Array.length program.procs)

let test_multiple_procs () =
  let program =
    make_emitter
      "const f := proc (x: Int) -> Int { x }; const g := proc (y: Int) -> Int \
       { y };"
  in
  check int "two procedures emitted" 5 (Array.length program.procs)

let test_no_procs () =
  let program = make_emitter "const x := 1;" in
  check int "no procedures emitted" 3 (Array.length program.procs)

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
