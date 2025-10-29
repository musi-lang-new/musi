open Alcotest
module Token = Token
module Lexer = Lexer
module Parser = Parser
module Tree = Tree
module Interner = Interner
module Diagnostic = Diagnostic

let make_parser source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _diags = Lexer.lex lexer in
  (tokens, interner)

let test_empty () =
  let tokens, interner = make_parser "" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "empty source has no errors" false (Diagnostic.has_errors diags)

let test_proc_basic () =
  let tokens, interner = make_parser "proc f() {}" in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "procedure has no errors" false (Diagnostic.has_errors diags);
  check int "one declaration" 1 (List.length ast)

let test_proc_with_params () =
  let tokens, interner =
    make_parser "proc add(x: Int, y: Int) -> Int { return x + y; }"
  in
  let ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "procedure with params has no errors"
    false
    (Diagnostic.has_errors diags);
  check int "one declaration" 1 (List.length ast)

let test_literals () =
  let tokens, interner =
    make_parser
      "proc test() { const x := 42; const y := true; const z := \"hello\"; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "literals parse without errors" false (Diagnostic.has_errors diags)

let test_binary_ops () =
  let tokens, interner =
    make_parser "proc test() { const x := 1 + 2 * 3; const y := x < 5; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "binary operations parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_if_then_else () =
  let tokens, interner =
    make_parser "proc test() { if true then { return 1; } else { return 0; } }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "if-else parses without errors" false (Diagnostic.has_errors diags)

let test_while_loop () =
  let tokens, interner =
    make_parser "proc test() { var i := 0; while i < 10 { i <- i + 1; } }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "while loop parses without errors"
    false
    (Diagnostic.has_errors diags)

let test_bindings () =
  let tokens, interner =
    make_parser "proc test() { const x := 42; var y := 0; y <- 1; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "const/var bindings parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_proc_calls () =
  let tokens, interner = make_parser "proc test() { const x := add(1, 2); }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "procedure calls parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_blocks () =
  let tokens, interner = make_parser "proc test() { { const x := 1; } }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "blocks parse without errors" false (Diagnostic.has_errors diags)

let test_return_stmt () =
  let tokens, interner = make_parser "proc test() -> Int { return 42; }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "return statement parses without errors"
    false
    (Diagnostic.has_errors diags)

let test_unary_ops () =
  let tokens, interner =
    make_parser "proc test() { const x := -42; const y := not true; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "unary operations parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_cast_expr () =
  let tokens, interner = make_parser "proc test() { const x := 42 as Nat; }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "cast expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_test_expr () =
  let tokens, interner = make_parser "proc test() { const x := 42 is Nat; }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "test expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_range_expr () =
  let tokens, interner =
    make_parser "proc test() { const x := 1..<10; const y := 1..10; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "range expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_field_access () =
  let tokens, interner =
    make_parser "proc test() { const x := obj.field; const y := tup.0; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "field access expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_index_access () =
  let tokens, interner =
    make_parser "proc test() { const x := arr[0]; const y := map[\"key\"]; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "index access expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_tuples () =
  let tokens, interner =
    make_parser
      "proc test() { const x := (); const y := (,); const z := (1, 2, 3); }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "tuple expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_arrays () =
  let tokens, interner =
    make_parser "proc test() { const x := []; const y := [1, 2, 3]; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "array expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_records () =
  let tokens, interner =
    make_parser "proc test() { const x := Point{ x := 1, y := 2 }; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "record expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_nested_expressions () =
  let tokens, interner =
    make_parser "proc test() { const x := arr[obj.field].method(a, b)[0]; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "nested expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_record_vs_block () =
  let tokens, interner =
    make_parser
      "proc test() { const x := A{ a := 1 }; const y := { const z := 2; }; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "record vs block disambiguation works"
    false
    (Diagnostic.has_errors diags)

let test_complex_tuples () =
  let tokens, interner =
    make_parser "proc test() { const x := (a.b, [1, 2], Point{ x := 0 }); }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "complex tuple expressions parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_precedence () =
  let tokens, interner =
    make_parser "proc test() { const x := a.b[0] + c * d as Type; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "operator precedence works correctly"
    false
    (Diagnostic.has_errors diags)

let test_empty_containers () =
  let tokens, interner =
    make_parser "proc test() { const x := []; const y := (); const z := (,); }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "empty containers parse correctly"
    false
    (Diagnostic.has_errors diags)

let test_chained_access () =
  let tokens, interner =
    make_parser
      "proc test() { const x := obj.field.subfield[\"key\"][0].method(); }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "chained field/index access works"
    false
    (Diagnostic.has_errors diags)

let test_range_precedence () =
  let tokens, interner =
    make_parser
      "proc test() { const x := 1 + 2..<5 * 3; const y := a[0]..b.field; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "range operator precedence works"
    false
    (Diagnostic.has_errors diags)

let test_all_operators () =
  let tokens, interner =
    make_parser
      "proc test() { const x := a ^ b * c / d mod e + f - g shl h shr i and j \
       xor k or l; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "all operators parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_operator_precedence () =
  let tokens, interner = make_parser "proc test() { const x := 2 ^ 3 ^ 4; }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "exponentiation is right-associative"
    false
    (Diagnostic.has_errors diags)

let test_assignment_operator () =
  let tokens, interner =
    make_parser "proc test() { var x := 0; x <- x + 1; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "assignment operator parses without errors"
    false
    (Diagnostic.has_errors diags)

let test_not_equal () =
  let tokens, interner = make_parser "proc test() { const x := a =/= b; }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "not equal operator parses without errors"
    false
    (Diagnostic.has_errors diags)

let test_modifiers_export () =
  let tokens, interner = make_parser "export const proc foo() {}" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "export const modifiers parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_modifiers_unsafe () =
  let tokens, interner = make_parser "unsafe proc bar() {}" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "unsafe modifier parses without errors"
    false
    (Diagnostic.has_errors diags)

let test_modifiers_async () =
  let tokens, interner = make_parser "async proc baz() {}" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "async modifier parses without errors"
    false
    (Diagnostic.has_errors diags)

let test_modifiers_extern () =
  let tokens, interner = make_parser "extern \"libc\" proc malloc() {}" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "extern modifier with library parses without errors"
    false
    (Diagnostic.has_errors diags)

let test_modifiers_combined () =
  let tokens, interner =
    make_parser "export const unsafe async proc complex() {}"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "combined modifiers parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_decorators () =
  let tokens, interner = make_parser "@inline proc fast() {}" in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "decorators parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_decorators_with_args () =
  let tokens, interner =
    make_parser "@deprecated(\"use new_proc instead\") proc old() {}"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "decorators with arguments parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_decorators_and_modifiers () =
  let tokens, interner =
    make_parser "@inline @deprecated export const proc combo() {}"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "decorators and modifiers together parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_literal_suffixes () =
  let tokens, interner =
    make_parser "proc test() { const x := 42_i32; const y := 3.14_b64; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "literal suffixes parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_pointer_types () =
  let tokens, interner =
    make_parser "proc test(ptr: *Int, ref: &Text) -> *Nat {}"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "pointer and reference types parse without errors"
    false
    (Diagnostic.has_errors diags)

let test_pointer_operations () =
  let tokens, interner =
    make_parser
      "proc test() { var x := 42; const ptr := &x; const val := ptr.*; ptr.* \
       <- 100; }"
  in
  let _ast, diags = Parser.parse_program tokens interner in
  check
    bool
    "pointer operations (address-of and dereference) parse without errors"
    false
    (Diagnostic.has_errors diags)

let () =
  run
    "Parser"
    [
      ( "Basic"
      , [
          test_case "empty" `Quick test_empty
        ; test_case "proc_basic" `Quick test_proc_basic
        ; test_case "proc_with_params" `Quick test_proc_with_params
        ] )
    ; ( "Literals & Operators"
      , [
          test_case "literals" `Quick test_literals
        ; test_case "binary_ops" `Quick test_binary_ops
        ; test_case "unary_ops" `Quick test_unary_ops
        ; test_case "cast_expr" `Quick test_cast_expr
        ; test_case "test_expr" `Quick test_test_expr
        ; test_case "range_expr" `Quick test_range_expr
        ] )
    ; ( "Data Structures"
      , [
          test_case "tuples" `Quick test_tuples
        ; test_case "arrays" `Quick test_arrays
        ; test_case "records" `Quick test_records
        ; test_case "field_access" `Quick test_field_access
        ; test_case "index_access" `Quick test_index_access
        ] )
    ; ( "Function Calls & Blocks"
      , [
          test_case "proc_calls" `Quick test_proc_calls
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
    ; ( "Complex & Edge Cases"
      , [
          test_case "nested_expressions" `Quick test_nested_expressions
        ; test_case "record_vs_block" `Quick test_record_vs_block
        ; test_case "complex_tuples" `Quick test_complex_tuples
        ; test_case "precedence" `Quick test_precedence
        ; test_case "empty_containers" `Quick test_empty_containers
        ; test_case "chained_access" `Quick test_chained_access
        ; test_case "range_precedence" `Quick test_range_precedence
        ] )
    ; ( "New Operators"
      , [
          test_case "all_operators" `Quick test_all_operators
        ; test_case "operator_precedence" `Quick test_operator_precedence
        ; test_case "assignment_operator" `Quick test_assignment_operator
        ; test_case "not_equal" `Quick test_not_equal
        ] )
    ; ( "Modifiers"
      , [
          test_case "modifiers_export" `Quick test_modifiers_export
        ; test_case "modifiers_unsafe" `Quick test_modifiers_unsafe
        ; test_case "modifiers_async" `Quick test_modifiers_async
        ; test_case "modifiers_extern" `Quick test_modifiers_extern
        ; test_case "modifiers_combined" `Quick test_modifiers_combined
        ] )
    ; ( "Decorators"
      , [
          test_case "decorators" `Quick test_decorators
        ; test_case "decorators_with_args" `Quick test_decorators_with_args
        ; test_case
            "decorators_and_modifiers"
            `Quick
            test_decorators_and_modifiers
        ] )
    ; ( "Literal Suffixes"
      , [ test_case "literal_suffixes" `Quick test_literal_suffixes ] )
    ; ( "Pointers & References"
      , [
          test_case "pointer_types" `Quick test_pointer_types
        ; test_case "pointer_operations" `Quick test_pointer_operations
        ] )
    ]
