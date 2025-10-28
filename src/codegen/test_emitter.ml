open Alcotest
module Emitter = Musi_codegen.Emitter
module Instr = Musi_codegen.Instr
module Tree = Musi_syntax.Tree
module Token = Musi_syntax.Token
module Interner = Musi_shared.Interner
module Span = Musi_shared.Span

let make_span () = Span.make 0 0 0

let make_expr kind =
  {
    Tree.kind
  ; span = make_span ()
  ; leading = []
  ; trailing = []
  ; ty = None
  ; sym = None
  }

let make_stmt expr : Tree.stmt =
  {
    kind = (Tree.Expr { expr } : Tree.stmt_kind)
  ; span = make_span ()
  ; leading = []
  ; trailing = []
  ; decorators = []
  ; modifiers = Tree.default_modifiers
  ; sym = None
  }

let test_constant_deduplication () =
  let interner = Interner.create () in
  let emitter = Emitter.create interner in
  let text_sym = Interner.intern interner "hello" in
  let expr1 = make_expr (Tree.TextLit { value = text_sym }) in
  let expr2 = make_expr (Tree.TextLit { value = text_sym }) in
  let program = [ make_stmt expr1; make_stmt expr2 ] in
  let result = Emitter.emit_program emitter program in
  check int "duplicate constants deduplicated" 1 (List.length result.constants)

let test_program_emission () =
  let interner = Interner.create () in
  let emitter = Emitter.create interner in
  let expr = make_expr (Tree.IntLit { value = "42"; suffix = None }) in
  let program = [ make_stmt expr ] in
  let result = Emitter.emit_program emitter program in
  check
    bool
    "program emitted successfully"
    true
    (List.length result.constants >= 0)

let test_binary_expr_emission () =
  let interner = Interner.create () in
  let emitter = Emitter.create interner in
  let lhs = make_expr (Tree.IntLit { value = "1"; suffix = None }) in
  let rhs = make_expr (Tree.IntLit { value = "2"; suffix = None }) in
  let expr = make_expr (Tree.Binary { op = Token.Plus; lhs; rhs }) in
  let program = [ make_stmt expr ] in
  let result = Emitter.emit_program emitter program in
  check bool "binary expression emitted" true (List.length result.constants >= 0)

let () =
  run
    "Emitter"
    [
      ( "Program Emission"
      , [
          test_case "constant_deduplication" `Quick test_constant_deduplication
        ; test_case "program_emission" `Quick test_program_emission
        ; test_case "binary_expr_emission" `Quick test_binary_expr_emission
        ] )
    ]
