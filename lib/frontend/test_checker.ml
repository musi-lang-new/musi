(** Checker tests for type inference and checking. *)

open Alcotest

let make_checker source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let resolver = Resolver.create interner in
  let _resolve_diags = Resolver.resolve resolver ast in
  let checker = Checker.create interner resolver in
  let diags = Checker.check checker ast in
  diags

let test_int_literal () =
  let diags = make_checker "42" in
  check bool "int literal type checks" false (Diagnostic.has_errors diags)

let test_bool_literal () =
  let diags = make_checker "true; false" in
  check bool "bool literals type check" false (Diagnostic.has_errors diags)

let test_text_literal () =
  let diags = make_checker "\"hello\"" in
  check bool "text literal type checks" false (Diagnostic.has_errors diags)

let test_unit_literal () =
  let diags = make_checker "()" in
  check bool "unit literal type checks" false (Diagnostic.has_errors diags)

let test_binary_int () =
  let diags = make_checker "1 + 2; 3 * 4; 5 - 6; 7 / 8" in
  check bool "int binary ops type check" false (Diagnostic.has_errors diags)

let test_binary_bool () =
  let diags = make_checker "true and false; true or false; true xor false" in
  check bool "bool binary ops type check" false (Diagnostic.has_errors diags)

let test_comparison () =
  let diags = make_checker "1 = 2; 3 =/= 4; 5 < 6; 7 > 8; 9 <= 10; 11 >= 12" in
  check bool "comparisons type check" false (Diagnostic.has_errors diags)

let test_unary_int () =
  let diags = make_checker "-42" in
  check bool "int unary op type checks" false (Diagnostic.has_errors diags)

let test_unary_bool () =
  let diags = make_checker "not true" in
  check bool "bool unary op type checks" false (Diagnostic.has_errors diags)

let test_if_expr () =
  let diags = make_checker "if true { 1 } else { 2 }" in
  check bool "if expr type checks" false (Diagnostic.has_errors diags)

let test_if_unit () =
  let diags = make_checker "if true { () }" in
  check bool "if without else type checks" false (Diagnostic.has_errors diags)

let test_block_expr () =
  let diags = make_checker "{ 1; 2; 3 }" in
  check bool "block expr type checks" false (Diagnostic.has_errors diags)

let test_empty_block () =
  let diags = make_checker "{}" in
  check bool "empty block type checks" false (Diagnostic.has_errors diags)

let test_binding () =
  let diags = make_checker "const x := 42; x" in
  check bool "binding type checks" false (Diagnostic.has_errors diags)

let test_proc_no_params () =
  let diags = make_checker "const f := proc () { 42 }" in
  check
    bool
    "proc without params type checks"
    false
    (Diagnostic.has_errors diags)

let test_proc_with_params () =
  let diags = make_checker "const f := proc (x: Int, y: Int) { x + y }" in
  check bool "proc with params type checks" false (Diagnostic.has_errors diags)

let test_proc_call () =
  let diags = make_checker "const f := proc () { 42 }; f()" in
  check bool "proc call type checks" false (Diagnostic.has_errors diags)

let test_proc_call_with_args () =
  let diags = make_checker "const f := proc (x: Int) { x }; f(42)" in
  check
    bool
    "proc call with args type checks"
    false
    (Diagnostic.has_errors diags)

let test_array_empty () =
  let diags = make_checker "[]" in
  check bool "empty array type checks" false (Diagnostic.has_errors diags)

let test_array_homogeneous () =
  let diags = make_checker "[1, 2, 3]" in
  check bool "homogeneous array type checks" false (Diagnostic.has_errors diags)

let test_tuple () =
  let diags = make_checker "(1, true, \"hello\")" in
  check bool "tuple type checks" false (Diagnostic.has_errors diags)

let test_cast () =
  let diags = make_checker "42 as Int" in
  check bool "cast type checks" false (Diagnostic.has_errors diags)

let test_cast_to_any () =
  let diags = make_checker "42 as Any" in
  check bool "cast to Any type checks" false (Diagnostic.has_errors diags)

let test_gradual_typing () =
  let diags = make_checker "const x: Any := 42; x" in
  check bool "gradual typing works" false (Diagnostic.has_errors diags)

let test_nested_expr () =
  let diags = make_checker "1 + 2 * 3 - 4 / 5" in
  check bool "nested expr type checks" false (Diagnostic.has_errors diags)

let test_complex_block () =
  let diags = make_checker "{ const x := 1; const y := 2; x + y }" in
  check bool "complex block type checks" false (Diagnostic.has_errors diags)

let test_extern_requires_unsafe () =
  let diags = make_checker "const f := extern \"intrinsic\" proc (x: Int);" in
  check
    bool
    "extern without unsafe produces error"
    true
    (Diagnostic.has_errors diags)

let test_unsafe_extern_valid () =
  let diags =
    make_checker "const f := unsafe extern \"intrinsic\" proc (x: Int);"
  in
  check bool "unsafe extern is valid" false (Diagnostic.has_errors diags)

let () =
  run
    "Checker"
    [
      ( "literals"
      , [
          test_case "int" `Quick test_int_literal
        ; test_case "bool" `Quick test_bool_literal
        ; test_case "text" `Quick test_text_literal
        ; test_case "unit" `Quick test_unit_literal
        ] )
    ; ( "operators"
      , [
          test_case "binary-int" `Quick test_binary_int
        ; test_case "binary-bool" `Quick test_binary_bool
        ; test_case "comparison" `Quick test_comparison
        ; test_case "unary-int" `Quick test_unary_int
        ; test_case "unary-bool" `Quick test_unary_bool
        ] )
    ; ( "control-flow"
      , [
          test_case "if-expr" `Quick test_if_expr
        ; test_case "if-unit" `Quick test_if_unit
        ; test_case "block" `Quick test_block_expr
        ; test_case "empty-block" `Quick test_empty_block
        ] )
    ; ( "bindings"
      , [
          test_case "simple" `Quick test_binding
        ; test_case "complex-block" `Quick test_complex_block
        ] )
    ; ( "procedures"
      , [
          test_case "no-params" `Quick test_proc_no_params
        ; test_case "with-params" `Quick test_proc_with_params
        ; test_case "call" `Quick test_proc_call
        ; test_case "call-with-args" `Quick test_proc_call_with_args
        ] )
    ; ( "compound"
      , [
          test_case "array-empty" `Quick test_array_empty
        ; test_case "array-homogeneous" `Quick test_array_homogeneous
        ; test_case "tuple" `Quick test_tuple
        ] )
    ; ( "gradual-typing"
      , [
          test_case "cast" `Quick test_cast
        ; test_case "cast-to-any" `Quick test_cast_to_any
        ; test_case "any-type" `Quick test_gradual_typing
        ] )
    ; ("expressions", [ test_case "nested" `Quick test_nested_expr ])
    ; ( "safety"
      , [
          test_case "extern-requires-unsafe" `Quick test_extern_requires_unsafe
        ; test_case "unsafe-extern-valid" `Quick test_unsafe_extern_valid
        ] )
    ]
