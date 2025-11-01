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
  check int "main + one procedure" 2 (Array.length program.procs)

let test_proc_call () =
  let program =
    make_emitter "const f := proc (x: Int) -> Int { x }; const y := f(1);"
  in
  check int "main + one procedure" 2 (Array.length program.procs)

let test_multiple_procs () =
  let program =
    make_emitter
      "const f := proc (x: Int) -> Int { x }; const g := proc (y: Int) -> Int \
       { y };"
  in
  check int "main + two procedures" 3 (Array.length program.procs)

let test_no_procs () =
  let program = make_emitter "const x := 1;" in
  check int "only main procedure" 1 (Array.length program.procs)

let test_extern_intrinsic_proc () =
  let program =
    make_emitter "const f := unsafe extern \"intrinsic\" proc (x: Int);"
  in
  check int "main + extern stub" 2 (Array.length program.procs);
  let extern_proc = program.procs.(1) in
  check bool "extern proc marked as external" true extern_proc.external_proc

let test_extern_intrinsic_call () =
  let program =
    make_emitter
      "const f := unsafe extern \"intrinsic\" proc (x: Int); const y := f(42);"
  in
  check int "main + extern stub" 2 (Array.length program.procs);
  let main_proc = program.procs.(0) in
  let has_call =
    List.exists (function Instr.Call _ -> true | _ -> false) main_proc.code
  in
  check bool "main contains call to extern" true has_call

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
        ; test_case "extern_intrinsic_proc" `Quick test_extern_intrinsic_proc
        ; test_case "extern_intrinsic_call" `Quick test_extern_intrinsic_call
        ] )
    ]
