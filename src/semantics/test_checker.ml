open Alcotest
module Lexer = Musi_syntax.Lexer
module Parser = Musi_syntax.Parser
module Binder = Musi_semantics.Binder
module Checker = Musi_semantics.Checker
module Symbol = Musi_semantics.Symbol
module Interner = Musi_shared.Interner
module Diagnostic = Musi_shared.Diagnostic

let make_checker source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let binder = Binder.create interner in
  let _bind_diags = Binder.bind_program binder ast in
  let checker = Checker.create interner binder.syms in
  let diags = Checker.check_program checker ast in
  diags

let test_type_mismatch () =
  let diags = make_checker "const x: Int := true;" in
  check bool "type mismatch error" true (Diagnostic.has_errors diags)

let test_binary_op_type () =
  let diags = make_checker "const x := 1 + 2;" in
  check bool "no errors for valid binary op" false (Diagnostic.has_errors diags)

let test_binary_op_mismatch () =
  let diags = make_checker "const x := 1 + true;" in
  check bool "binary op type mismatch" true (Diagnostic.has_errors diags)

let test_if_condition_bool () =
  let diags = make_checker "const x := if 1 { 2 } else { 3 };" in
  check bool "if condition must be bool" true (Diagnostic.has_errors diags)

let test_if_branch_mismatch () =
  let diags = make_checker "const x := if true { 1 } else { true };" in
  check bool "if branch type mismatch" true (Diagnostic.has_errors diags)

let test_proc_call_arg_count () =
  let diags =
    make_checker "const f := proc (x: Int) -> Int { x }; const y := f(1, 2);"
  in
  check bool "proc call arg count mismatch" true (Diagnostic.has_errors diags)

let test_proc_call_arg_type () =
  let diags =
    make_checker "const f := proc (x: Int) -> Int { x }; const y := f(true);"
  in
  check bool "proc call arg type mismatch" true (Diagnostic.has_errors diags)

let test_proc_return_type () =
  let diags = make_checker "const f := proc () -> Int { true };" in
  check bool "proc body type mismatch" true (Diagnostic.has_errors diags)

let test_proc_explicit_return () =
  let diags = make_checker "const f := proc () -> Int { return true; };" in
  check bool "explicit return type mismatch" true (Diagnostic.has_errors diags)

let test_return_outside_proc () =
  let diags = make_checker "return 1;" in
  check bool "return outside proc error" true (Diagnostic.has_errors diags)

let test_assignment_type () =
  let diags = make_checker "var x := 1; x <- true;" in
  check bool "assignment type mismatch" true (Diagnostic.has_errors diags)

let test_valid_assignment () =
  let diags = make_checker "var x := 1; x <- 2;" in
  check
    bool
    "no errors for valid assignment"
    false
    (Diagnostic.has_errors diags)

let test_valid_proc () =
  let diags = make_checker "const f := proc (x: Int) -> Int { x };" in
  check bool "no errors for valid proc" false (Diagnostic.has_errors diags)

let () =
  run
    "Checker"
    [
      ( "Type Inference"
      , [
          test_case "type_mismatch" `Quick test_type_mismatch
        ; test_case "binary_op_type" `Quick test_binary_op_type
        ; test_case "binary_op_mismatch" `Quick test_binary_op_mismatch
        ] )
    ; ( "Control Flow"
      , [
          test_case "if_condition_bool" `Quick test_if_condition_bool
        ; test_case "if_branch_mismatch" `Quick test_if_branch_mismatch
        ] )
    ; ( "Procedures"
      , [
          test_case "proc_call_arg_count" `Quick test_proc_call_arg_count
        ; test_case "proc_call_arg_type" `Quick test_proc_call_arg_type
        ; test_case "proc_return_type" `Quick test_proc_return_type
        ; test_case "proc_explicit_return" `Quick test_proc_explicit_return
        ; test_case "return_outside_proc" `Quick test_return_outside_proc
        ; test_case "valid_proc" `Quick test_valid_proc
        ] )
    ; ( "Assignment"
      , [
          test_case "assignment_type" `Quick test_assignment_type
        ; test_case "valid_assignment" `Quick test_valid_assignment
        ] )
    ]
