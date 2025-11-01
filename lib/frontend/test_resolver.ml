(** Resolver tests for name resolution and scoping. *)

open Alcotest

let make_resolver source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let resolver = Resolver.create interner in
  let diags = Resolver.resolve resolver ast in
  (diags, interner)

let test_simple_binding () =
  let diags, _ = make_resolver "const x := 42" in
  check bool "simple binding has no errors" false (Diagnostic.has_errors diags)

let test_undefined_name () =
  let diags, _ = make_resolver "const x := y" in
  check bool "undefined name produces error" true (Diagnostic.has_errors diags)

let test_duplicate_definition () =
  let diags, _ = make_resolver "const x := 1; const x := 2" in
  check
    bool
    "duplicate definition produces error"
    true
    (Diagnostic.has_errors diags)

let test_shadowing () =
  let diags, _ = make_resolver "const x := 1; { const x := 2 }" in
  check bool "shadowing is allowed" false (Diagnostic.has_errors diags)

let test_proc_definition () =
  let diags, _ = make_resolver "const f := proc (x: Int) { x }" in
  check bool "proc definition has no errors" false (Diagnostic.has_errors diags)

let test_proc_call () =
  let diags, _ = make_resolver "const f := proc () { 42 }; const y := f()" in
  check bool "proc call resolves" false (Diagnostic.has_errors diags)

let test_undefined_proc () =
  let diags, _ = make_resolver "const y := f()" in
  check bool "undefined proc produces error" true (Diagnostic.has_errors diags)

let test_param_scope () =
  let diags, _ = make_resolver "const f := proc (x: Int) { x + 1 }" in
  check bool "param in scope" false (Diagnostic.has_errors diags)

let test_param_out_of_scope () =
  let diags, _ = make_resolver "const f := proc (x: Int) { x }; const y := x" in
  check
    bool
    "param out of scope produces error"
    true
    (Diagnostic.has_errors diags)

let test_block_scope () =
  let diags, _ = make_resolver "{ const x := 1 }; const y := x" in
  check bool "block scope isolates bindings" true (Diagnostic.has_errors diags)

let test_nested_blocks () =
  let diags, _ =
    make_resolver "const x := 1; { const y := x; { const z := y } }"
  in
  check bool "nested blocks resolve" false (Diagnostic.has_errors diags)

let test_for_loop_scope () =
  let diags, _ = make_resolver "const xs := [1, 2, 3]; for x in xs { x }" in
  check bool "for loop variable in scope" false (Diagnostic.has_errors diags)

let test_match_pattern_scope () =
  let diags, _ = make_resolver "const x := 1; match x { case y -> y }" in
  check
    bool
    "match pattern variable in scope"
    false
    (Diagnostic.has_errors diags)

let test_multiple_procs () =
  let diags, _ =
    make_resolver "const f := proc () { 1 }; const g := proc () { f() }"
  in
  check bool "multiple procs resolve" false (Diagnostic.has_errors diags)

let test_recursive_call () =
  let diags, _ =
    make_resolver "const f := proc (n: Int) { if n = 0 { 1 } else { n - 1 } }"
  in
  check bool "proc body resolves" false (Diagnostic.has_errors diags)

let test_extern_intrinsic_proc () =
  let diags, _ =
    make_resolver "const f := unsafe extern \"intrinsic\" proc (x: Int);"
  in
  check
    bool
    "extern intrinsic proc resolves"
    false
    (Diagnostic.has_errors diags)

let test_extern_intrinsic_call () =
  let diags, _ =
    make_resolver
      "const f := unsafe extern \"intrinsic\" proc (x: Int); const y := f(42)"
  in
  check
    bool
    "call to extern intrinsic resolves"
    false
    (Diagnostic.has_errors diags)

let test_extern_intrinsic_symbol () =
  let source = "const f := unsafe extern \"intrinsic\" proc (x: Int);" in
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _lex_diags = Lexer.lex lexer in
  let ast, _parse_diags = Parser.parse_program tokens interner in
  let resolver = Resolver.create interner in
  let _diags = Resolver.resolve resolver ast in
  let f_sym = Interner.intern interner "f" in
  let intrinsic_sym = Interner.intern interner "intrinsic" in
  let sym_opt = Resolver.lookup resolver f_sym in
  match sym_opt with
  | Some sym -> (
    match sym.kind with
    | Resolver.SymProc { params; extern_ } ->
      check int "extern proc has 1 param" 1 params;
      check
        bool
        "extern proc has intrinsic ABI"
        true
        (extern_ = Some intrinsic_sym)
    | _ -> fail "expected SymProc")
  | None -> fail "symbol not found"

let () =
  run
    "Resolver"
    [
      ( "bindings"
      , [
          test_case "simple" `Quick test_simple_binding
        ; test_case "undefined" `Quick test_undefined_name
        ; test_case "duplicate" `Quick test_duplicate_definition
        ; test_case "shadowing" `Quick test_shadowing
        ] )
    ; ( "procedures"
      , [
          test_case "definition" `Quick test_proc_definition
        ; test_case "call" `Quick test_proc_call
        ; test_case "undefined" `Quick test_undefined_proc
        ; test_case "multiple" `Quick test_multiple_procs
        ; test_case "recursive" `Quick test_recursive_call
        ; test_case "extern-intrinsic" `Quick test_extern_intrinsic_proc
        ; test_case "extern-intrinsic-call" `Quick test_extern_intrinsic_call
        ; test_case
            "extern-intrinsic-symbol"
            `Quick
            test_extern_intrinsic_symbol
        ] )
    ; ( "scoping"
      , [
          test_case "param-in-scope" `Quick test_param_scope
        ; test_case "param-out-of-scope" `Quick test_param_out_of_scope
        ; test_case "block-scope" `Quick test_block_scope
        ; test_case "nested-blocks" `Quick test_nested_blocks
        ; test_case "for-loop" `Quick test_for_loop_scope
        ; test_case "match-pattern" `Quick test_match_pattern_scope
        ] )
    ]
