(** Parser tests for expression-oriented AST. *)

open Alcotest

let make_parser source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _diags = Lexer.lex lexer in
  (tokens, interner)

let test_empty () =
  let tokens, interner = make_parser "" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "empty source has no errors" false (Diagnostic.has_errors diags)

let test_literals () =
  let tokens, interner = make_parser "42; true; false; \"hello\"" in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "literals parse without errors" false (Diagnostic.has_errors diags);
  check int "four literals" 4 (List.length ast)

let test_binary_ops () =
  let tokens, interner = make_parser "1 + 2; 3 * 4; 5 - 6" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "binary ops parse" false (Diagnostic.has_errors diags)

let test_unary_ops () =
  let tokens, interner = make_parser "-42; not true" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "unary ops parse" false (Diagnostic.has_errors diags)

let test_precedence () =
  let tokens, interner = make_parser "1 + 2 * 3; 2^3^4" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "precedence works" false (Diagnostic.has_errors diags)

let test_call () =
  let tokens, interner = make_parser "f(); g(1, 2, 3)" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "calls parse" false (Diagnostic.has_errors diags)

let test_field_access () =
  let tokens, interner = make_parser "obj.field; tup.0" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "field access parses" false (Diagnostic.has_errors diags)

let test_index_access () =
  let tokens, interner = make_parser "arr[0]; map[\"key\"]" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "index access parses" false (Diagnostic.has_errors diags)

let test_cast_test () =
  let tokens, interner = make_parser "x as Int; y is Bool" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "cast/test parse" false (Diagnostic.has_errors diags)

let test_range () =
  let tokens, interner = make_parser "1..10; 0..<5" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "ranges parse" false (Diagnostic.has_errors diags)

let test_assign () =
  let tokens, interner = make_parser "x <- 42" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "assignment parses" false (Diagnostic.has_errors diags)

let test_bind () =
  let tokens, interner = make_parser "const x := 42; var y := 0" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "bindings parse" false (Diagnostic.has_errors diags)

let test_bind_with_type () =
  let tokens, interner = make_parser "const x: Int := 42" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "typed binding parses" false (Diagnostic.has_errors diags)

let test_return () =
  let tokens, interner = make_parser "return; return 42" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "return parses" false (Diagnostic.has_errors diags)

let test_break () =
  let tokens, interner = make_parser "break; break 42" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "break parses" false (Diagnostic.has_errors diags)

let test_continue () =
  let tokens, interner = make_parser "continue" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "continue parses" false (Diagnostic.has_errors diags)

let test_if () =
  let tokens, interner = make_parser "if true { 1 }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "if parses" false (Diagnostic.has_errors diags)

let test_if_else () =
  let tokens, interner = make_parser "if true { 1 } else { 0 }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "if-else parses" false (Diagnostic.has_errors diags)

let test_while () =
  let tokens, interner = make_parser "while true { break }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "while parses" false (Diagnostic.has_errors diags)

let test_for () =
  let tokens, interner = make_parser "for x in xs { x }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "for parses" false (Diagnostic.has_errors diags)

let test_match () =
  let tokens, interner = make_parser "match x { case 0 -> 1, case _ -> 2 }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "match parses" false (Diagnostic.has_errors diags)

let test_match_with_guard () =
  let tokens, interner = make_parser "match x { case n if n > 0 -> 1 }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "match with guard parses" false (Diagnostic.has_errors diags)

let test_proc () =
  let tokens, interner = make_parser "proc () { 42 }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "proc parses" false (Diagnostic.has_errors diags)

let test_proc_with_params () =
  let tokens, interner = make_parser "proc (x: Int, y: Int) { x + y }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "proc with params parses" false (Diagnostic.has_errors diags)

let test_proc_with_return_type () =
  let tokens, interner = make_parser "proc () -> Int { 42 }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "proc with return type parses" false (Diagnostic.has_errors diags)

let test_block () =
  let tokens, interner = make_parser "{ const x := 1; x + 1 }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "block parses" false (Diagnostic.has_errors diags)

let test_empty_block () =
  let tokens, interner = make_parser "{}" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "empty block parses" false (Diagnostic.has_errors diags)

let test_tuple () =
  let tokens, interner = make_parser "(); (1,); (1, 2, 3)" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "tuples parse" false (Diagnostic.has_errors diags)

let test_array () =
  let tokens, interner = make_parser "[]; [1, 2, 3]" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "arrays parse" false (Diagnostic.has_errors diags)

let test_patterns () =
  let tokens, interner =
    make_parser "match x { case const y -> y, case _ -> 0 }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "patterns parse" false (Diagnostic.has_errors diags)

let test_nested_calls () =
  let tokens, interner = make_parser "f(g(h(x)))" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "nested calls parse" false (Diagnostic.has_errors diags)

let test_chained_access () =
  let tokens, interner = make_parser "obj.field[0].method()" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "chained access parses" false (Diagnostic.has_errors diags)

let test_complex_expr () =
  let tokens, interner = make_parser "f(x.y[0] + 1, g(2) * 3)" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "complex expr parses" false (Diagnostic.has_errors diags)

let test_all_operators () =
  let tokens, interner =
    make_parser "a^b * c / d mod e + f - g shl h shr i and j xor k or l"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "all operators parse" false (Diagnostic.has_errors diags)

let () =
  run
    "Parser"
    [
      ( "literals"
      , [
          test_case "empty" `Quick test_empty
        ; test_case "literals" `Quick test_literals
        ] )
    ; ( "operators"
      , [
          test_case "binary" `Quick test_binary_ops
        ; test_case "unary" `Quick test_unary_ops
        ; test_case "precedence" `Quick test_precedence
        ; test_case "all" `Quick test_all_operators
        ] )
    ; ( "access"
      , [
          test_case "call" `Quick test_call
        ; test_case "field" `Quick test_field_access
        ; test_case "index" `Quick test_index_access
        ; test_case "nested" `Quick test_nested_calls
        ; test_case "chained" `Quick test_chained_access
        ] )
    ; ( "expressions"
      , [
          test_case "cast/test" `Quick test_cast_test
        ; test_case "range" `Quick test_range
        ; test_case "assign" `Quick test_assign
        ; test_case "complex" `Quick test_complex_expr
        ] )
    ; ( "bindings"
      , [
          test_case "bind" `Quick test_bind
        ; test_case "typed" `Quick test_bind_with_type
        ] )
    ; ( "control"
      , [
          test_case "return" `Quick test_return
        ; test_case "break" `Quick test_break
        ; test_case "continue" `Quick test_continue
        ; test_case "if" `Quick test_if
        ; test_case "if-else" `Quick test_if_else
        ; test_case "while" `Quick test_while
        ; test_case "for" `Quick test_for
        ; test_case "match" `Quick test_match
        ; test_case "match-guard" `Quick test_match_with_guard
        ] )
    ; ( "procedures"
      , [
          test_case "basic" `Quick test_proc
        ; test_case "params" `Quick test_proc_with_params
        ; test_case "return-type" `Quick test_proc_with_return_type
        ] )
    ; ( "blocks"
      , [
          test_case "block" `Quick test_block
        ; test_case "empty" `Quick test_empty_block
        ] )
    ; ( "compound"
      , [
          test_case "tuple" `Quick test_tuple
        ; test_case "array" `Quick test_array
        ] )
    ; ("patterns", [ test_case "basic" `Quick test_patterns ])
    ]
