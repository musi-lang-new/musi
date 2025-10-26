open Alcotest
module Token = Musi_syntax.Token
module Lexer = Musi_syntax.Lexer
module Parser = Musi_syntax.Parser
module Tree = Musi_syntax.Tree
module Interner = Musi_shared.Interner
module Diagnostic = Musi_shared.Diagnostic

let make_parser source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _diags = Lexer.lex lexer in
  (tokens, interner)

let test_empty () =
  let tokens, interner = make_parser "" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "empty source has no errors" false (Diagnostic.has_errors diags)

let test_func_basic () =
  let tokens, interner = make_parser "func f() {}" in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "function has no errors" false (Diagnostic.has_errors diags);
  check int "one declaration" 1 (List.length ast)

let test_func_with_params () =
  let tokens, interner =
    make_parser "func add(x: Int, y: Int) -> Int { return x + y; }"
  in
  let ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "function with params has no errors"
    false
    (Diagnostic.has_errors diags);
  check int "one declaration" 1 (List.length ast)

let test_literals () =
  let tokens, interner =
    make_parser
      "func test() { const x := 42; const y := true; const z := \"hello\"; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "literals parse without errors" false (Diagnostic.has_errors diags)

let test_binary_ops () =
  let tokens, interner =
    make_parser "func test() { const x := 1 + 2 * 3; const y := x < 5; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "binary operations parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_if_then_else () =
  let tokens, interner =
    make_parser "func test() { if true then { return 1; } else { return 0; } }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "if-else parses without errors" false (Diagnostic.has_errors diags)

let test_while_loop () =
  let tokens, interner =
    make_parser "func test() { var i := 0; while i < 10 { i <- i + 1; } }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "while loop parses without errors"
    false
    (Diagnostic.has_errors diags)

let test_bindings () =
  let tokens, interner =
    make_parser "func test() { const x := 42; var y := 0; y <- 1; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "const/var bindings parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_func_calls () =
  let tokens, interner = make_parser "func test() { const x := add(1, 2); }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "function calls parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_blocks () =
  let tokens, interner = make_parser "func test() { { const x := 1; } }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "blocks parse without errors" false (Diagnostic.has_errors diags)

let test_return_stmt () =
  let tokens, interner = make_parser "func test() -> Int { return 42; }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "return statement parses without errors"
    false
    (Diagnostic.has_errors diags)

let test_unary_ops () =
  let tokens, interner =
    make_parser "func test() { const x := -42; const y := not true; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "unary operations parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_cast_expr () =
  let tokens, interner = make_parser "func test() { const x := 42 as Nat; }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "cast expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_test_expr () =
  let tokens, interner = make_parser "func test() { const x := 42 is Nat; }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "test expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_range_expr () =
  let tokens, interner =
    make_parser "func test() { const x := 1..10; const y := 1...10; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "range expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let () =
  run
    "Parser"
    [
      ( "Basic"
      , [
          test_case "empty" `Quick test_empty
        ; test_case "func_basic" `Quick test_func_basic
        ; test_case "func_with_params" `Quick test_func_with_params
        ] )
    ; ( "Expressions"
      , [
          test_case "literals" `Quick test_literals
        ; test_case "binary_ops" `Quick test_binary_ops
        ; test_case "unary_ops" `Quick test_unary_ops
        ; test_case "cast_expr" `Quick test_cast_expr
        ; test_case "test_expr" `Quick test_test_expr
        ; test_case "range_expr" `Quick test_range_expr
        ; test_case "func_calls" `Quick test_func_calls
        ; test_case "blocks" `Quick test_blocks
        ] )
    ; ( "Statements"
      , [
          test_case "bindings" `Quick test_bindings
        ; test_case "return_stmt" `Quick test_return_stmt
        ] )
    ; ( "Control Flow"
      , [
          test_case "if_then_else" `Quick test_if_then_else
        ; test_case "while_loop" `Quick test_while_loop
        ] )
    ]
