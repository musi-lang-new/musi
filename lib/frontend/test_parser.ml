(** Parser tests for expression-oriented AST. *)

open Alcotest

let make_parser source =
  let interner = Interner.create () in
  let lexer = Lexer.make 0 source interner in
  let tokens, _diags = Lexer.lex lexer in
  (tokens, interner)

let parse_single source =
  let tokens, interner = make_parser source in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "no parse errors" false (Diagnostic.has_errors diags);
  check bool "has nodes" true (List.length ast > 0);
  (List.hd ast, interner)

(* Node kind checkers *)
let is_int_lit = function Node.ExprIntLit _ -> true | _ -> false
let is_bool_lit = function Node.ExprBoolLit _ -> true | _ -> false
let is_text_lit = function Node.ExprTextLit _ -> true | _ -> false
let is_unit_lit = function Node.ExprUnitLit -> true | _ -> false
let is_ident = function Node.ExprIdent _ -> true | _ -> false
let is_binary = function Node.ExprBinary _ -> true | _ -> false
let is_unary = function Node.ExprUnary _ -> true | _ -> false
let is_call = function Node.ExprCall _ -> true | _ -> false
let is_if = function Node.ExprIf _ -> true | _ -> false
let is_binding = function Node.ExprBinding _ -> true | _ -> false
let is_proc = function Node.ExprProc _ -> true | _ -> false
let is_assign = function Node.ExprAssign _ -> true | _ -> false
let is_tuple = function Node.ExprTuple _ -> true | _ -> false
let is_array = function Node.ExprArray _ -> true | _ -> false

(* Node extractors *)
let as_call = function
  | Node.ExprCall { callee; args } -> (callee, args)
  | _ -> fail "expected ExprCall"

let as_binding = function
  | Node.ExprBinding { mutable_; weakness; pat; ty; init } ->
    (mutable_, weakness, pat, ty, init)
  | _ -> fail "expected ExprBinding"

let as_proc = function
  | Node.ExprProc { params; ret_ty; body; asyncness; unsafe_; external_ } ->
    (params, ret_ty, body, asyncness, unsafe_, external_)
  | _ -> fail "expected ExprProc"

let as_if = function
  | Node.ExprIf { cond; then_br; else_br } -> (cond, then_br, else_br)
  | _ -> fail "expected ExprIf"

let test_empty () =
  let tokens, interner = make_parser "" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "empty source has no errors" false (Diagnostic.has_errors diags)

let test_literals () =
  let tokens, interner = make_parser "42; true; false; \"hello\"" in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "literals parse without errors" false (Diagnostic.has_errors diags);
  check int "four literals" 4 (List.length ast);
  check bool "first is ExprIntLit" true (is_int_lit (List.nth ast 0).Node.kind);
  check
    bool
    "second is ExprBoolLit"
    true
    (is_bool_lit (List.nth ast 1).Node.kind);
  check
    bool
    "third is ExprBoolLit"
    true
    (is_bool_lit (List.nth ast 2).Node.kind);
  check
    bool
    "fourth is ExprTextLit"
    true
    (is_text_lit (List.nth ast 3).Node.kind)

let test_binary_ops () =
  let tokens, interner = make_parser "1 + 2; 3 * 4; 5 - 6" in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "binary ops parse" false (Diagnostic.has_errors diags);
  List.iter
    (fun (n : Node.node) -> check bool "is ExprBinary" true (is_binary n.kind))
    ast

let test_unary_ops () =
  let tokens, interner = make_parser "-42; not true" in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "unary ops parse" false (Diagnostic.has_errors diags);
  List.iter
    (fun (n : Node.node) -> check bool "is ExprUnary" true (is_unary n.kind))
    ast

let test_precedence () =
  let tokens, interner = make_parser "1 + 2 * 3; 2^3^4" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "precedence works" false (Diagnostic.has_errors diags)

let test_call () =
  let node, _ = parse_single "f()" in
  check bool "is ExprCall" true (is_call node.Node.kind);
  let callee, args = as_call node.Node.kind in
  check bool "callee is ExprIdent" true (is_ident callee.Node.kind);
  check int "no args" 0 (List.length args.Node.items)

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
  let node, _ = parse_single "x <- 42" in
  check bool "is ExprAssign" true (is_assign node.Node.kind)

let test_bind () =
  let node, _ = parse_single "const x := 42" in
  check bool "is ExprBinding" true (is_binding node.Node.kind);
  let mutable_, _, _, _, _ = as_binding node.Node.kind in
  check bool "is ExprBinding (mutable=false)" false mutable_

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
  let node, _ = parse_single "if true { 1 }" in
  check bool "is if" true (is_if node.Node.kind);
  let _, _, else_br = as_if node.Node.kind in
  check bool "no else" true (Option.is_none else_br)

let test_if_else () =
  let node, _ = parse_single "if true { 1 } else { 0 }" in
  check bool "is if" true (is_if node.Node.kind);
  let _, _, else_br = as_if node.Node.kind in
  check bool "has else" true (Option.is_some else_br)

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
  let node, _ = parse_single "proc () { 42 }" in
  check bool "is ExprProc" true (is_proc node.Node.kind);
  let params, _, body, _, _, _ = as_proc node.Node.kind in
  check int "no params" 0 (List.length params.Node.items);
  check bool "has body" true (Option.is_some body)

let test_proc_with_params () =
  let node, _ = parse_single "proc (x: Int, y: Int) { x + y }" in
  check bool "is ExprProc" true (is_proc node.Node.kind);
  let params, _, _, _, _, _ = as_proc node.Node.kind in
  check int "two params" 2 (List.length params.Node.items)

let test_proc_with_return_type () =
  let node, _ = parse_single "proc () -> Int { 42 }" in
  check bool "is ExprProc" true (is_proc node.Node.kind);
  let _, ret_ty, _, _, _, _ = as_proc node.Node.kind in
  check bool "has return type" true (Option.is_some ret_ty)

let test_block () =
  let tokens, interner = make_parser "{ const x := 1; x + 1 }" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "block parses" false (Diagnostic.has_errors diags)

let test_empty_block () =
  let tokens, interner = make_parser "{}" in
  let _ast, diags = Parser.parse_program tokens interner in
  check bool "empty block parses" false (Diagnostic.has_errors diags)

let test_tuple () =
  let node, _ = parse_single "()" in
  check bool "unit is ExprUnitLit" true (is_unit_lit node.Node.kind);
  let node, _ = parse_single "(1, 2, 3)" in
  check bool "is ExprTuple" true (is_tuple node.Node.kind)

let test_array () =
  let node, _ = parse_single "[1, 2, 3]" in
  check bool "is ExprArray" true (is_array node.Node.kind)

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

let test_import_named () =
  let tokens, interner =
    make_parser "import { writeln } from \"stdlib/io.ms\""
  in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "import parses" false (Diagnostic.has_errors diags);
  check int "one import" 1 (List.length ast);
  match (List.hd ast).Node.kind with
  | Node.ExprImport { source; kind = Node.Named { items } } ->
    check int "one item" 1 (List.length items);
    check
      string
      "source path"
      "stdlib/io.ms"
      (Interner.to_string interner source)
  | _ -> fail "expected ExprImport node"

let test_import_multiple () =
  let tokens, interner =
    make_parser "import { writeln, write } from \"io.ms\""
  in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "import multiple parses" false (Diagnostic.has_errors diags);
  match (List.hd ast).Node.kind with
  | Node.ExprImport { kind = Node.Named { items }; _ } ->
    check int "two items" 2 (List.length items)
  | _ -> fail "expected ExprImport node"

let test_import_with_alias () =
  let tokens, interner =
    make_parser "import { writeln as print } from \"io.ms\""
  in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "import with alias parses" false (Diagnostic.has_errors diags);
  match (List.hd ast).Node.kind with
  | Node.ExprImport { kind = Node.Named { items }; _ } ->
    let item = List.hd items in
    check bool "has alias" true (Option.is_some item.Node.alias);
    check
      string
      "alias name"
      "print"
      (Interner.to_string interner (Option.get item.alias))
  | _ -> fail "expected ExprImport node"

let test_export_named () =
  let tokens, interner = make_parser "export { writeln, write }" in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "export parses" false (Diagnostic.has_errors diags);
  match (List.hd ast).Node.kind with
  | Node.ExprExport { source; kind = Node.Named { items } } ->
    check bool "no source" true (Option.is_none source);
    check int "two items" 2 (List.length items)
  | _ -> fail "expected ExprExport node"

let test_export_modifier () =
  let tokens, interner = make_parser "export const x := 42" in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "export modifier parses" false (Diagnostic.has_errors diags);
  let node = List.hd ast in
  check bool "node is exported" true node.Node.exported;
  match node.Node.kind with
  | Node.ExprBinding _ -> ()
  | _ -> fail "expected ExprBinding node"

let test_export_proc () =
  let tokens, interner = make_parser "export const foo := proc() { 42 }" in
  let ast, diags = Parser.parse_program tokens interner in
  check bool "export proc parses" false (Diagnostic.has_errors diags);
  let node = List.hd ast in
  check bool "proc is exported" true node.Node.exported;
  match node.Node.kind with
  | Node.ExprBinding { init; _ } -> (
    match init.Node.kind with
    | Node.ExprProc _ -> ()
    | _ -> fail "expected ExprProc in binding")
  | _ -> fail "expected ExprBinding node"

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
    ; ( "modules"
      , [
          test_case "import-named" `Quick test_import_named
        ; test_case "import-multiple" `Quick test_import_multiple
        ; test_case "import-alias" `Quick test_import_with_alias
        ; test_case "export-named" `Quick test_export_named
        ; test_case "export-modifier" `Quick test_export_modifier
        ; test_case "export-proc" `Quick test_export_proc
        ] )
    ]
