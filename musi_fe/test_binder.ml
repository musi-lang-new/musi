open Alcotest
module Lexer = Lexer
module Parser = Parser
module Binder = Binder
module Interner = Interner
module Diagnostic = Diagnostic

let make_binder source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let binder = Binder.create interner in
  let diags = Binder.bind_program binder ast in
  diags

let test_undeclared_identifier () =
  let diags = make_binder "const x := y;" in
  check bool "undeclared identifier error" true (Diagnostic.has_errors diags)

let test_duplicate_definition () =
  let diags = make_binder "const x := 1; const x := 2;" in
  check bool "duplicate definition error" true (Diagnostic.has_errors diags)

let test_valid_binding () =
  let diags = make_binder "const x := 1; const y := x;" in
  check bool "no errors for valid binding" false (Diagnostic.has_errors diags)

let test_immutable_assignment () =
  let diags = make_binder "const x := 1; x <- 2;" in
  check bool "immutable assignment error" true (Diagnostic.has_errors diags)

let test_mutable_assignment () =
  let diags = make_binder "var x := 1; x <- 2;" in
  check
    bool
    "no errors for mutable assignment"
    false
    (Diagnostic.has_errors diags)

let test_proc_binding () =
  let diags = make_binder "const f := proc (x: Int) -> Int { x };" in
  check bool "no errors for proc binding" false (Diagnostic.has_errors diags)

let test_proc_call () =
  let diags =
    make_binder "const f := proc (x: Int) -> Int { x }; const y := f(1);"
  in
  check bool "no errors for proc call" false (Diagnostic.has_errors diags)

let test_scope () =
  let diags = make_binder "{ const x := 1; } const y := x;" in
  check bool "out of scope error" true (Diagnostic.has_errors diags)

let () =
  run
    "Binder"
    [
      ( "Name Resolution"
      , [
          test_case "undeclared_identifier" `Quick test_undeclared_identifier
        ; test_case "duplicate_definition" `Quick test_duplicate_definition
        ; test_case "valid_binding" `Quick test_valid_binding
        ] )
    ; ( "Mutability"
      , [
          test_case "immutable_assignment" `Quick test_immutable_assignment
        ; test_case "mutable_assignment" `Quick test_mutable_assignment
        ] )
    ; ( "Procedures"
      , [
          test_case "proc_binding" `Quick test_proc_binding
        ; test_case "proc_call" `Quick test_proc_call
        ] )
    ; ("Scope", [ test_case "scope" `Quick test_scope ])
    ]
