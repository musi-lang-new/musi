open Alcotest
module Token = Musi_syntax.Token
module Lexer = Musi_syntax.Lexer
module Interner = Musi_shared.Interner
module Diagnostic = Musi_shared.Diagnostic

let make_lexer source =
  let interner = Interner.create () in
  Lexer.make 0 source interner

let test_empty () =
  let lexer = make_lexer "" in
  let tokens, _diags = Lexer.lex lexer in
  check int "empty source has only EOF" 1 (List.length tokens);
  match tokens with
  | [ tok ] -> check bool "EOF token" true (tok.Token.kind = Token.Eof)
  | _ -> fail "expected single EOF token"

let test_whitespace () =
  let lexer = make_lexer "   \t\r  " in
  let tokens, _diags = Lexer.lex lexer in
  check int "whitespace tokens plus EOF" 2 (List.length tokens);
  match tokens with
  | [ ws; eof ] ->
    check bool "whitespace token" true (ws.Token.kind = Token.Whitespace);
    check bool "EOF token" true (eof.Token.kind = Token.Eof)
  | _ -> fail "expected whitespace and EOF"

let test_newlines () =
  let lexer = make_lexer "\n\n\n" in
  let tokens, _diags = Lexer.lex lexer in
  check int "three newlines plus EOF" 4 (List.length tokens);
  List.iter
    (fun tok ->
      if tok.Token.kind <> Token.Eof then
        check bool "newline token" true (tok.Token.kind = Token.Newline))
    (List.rev (List.tl (List.rev tokens)))

let test_keywords () =
  let lexer = make_lexer "func if else while for" in
  let tokens, _diags = Lexer.lex lexer in
  let kinds = List.map (fun tok -> tok.Token.kind) tokens in
  let expected =
    [
      Token.KwFunc
    ; Token.Whitespace
    ; Token.KwIf
    ; Token.Whitespace
    ; Token.KwElse
    ; Token.Whitespace
    ; Token.KwWhile
    ; Token.Whitespace
    ; Token.KwFor
    ; Token.Eof
    ]
  in
  let check_kinds expected actual =
    check int "token count" (List.length expected) (List.length actual);
    List.iter2
      (fun e a -> check bool "token match" true (e = a))
      expected
      actual
  in
  check_kinds expected kinds

let test_identifiers () =
  let lexer = make_lexer "hello world_123 _private CamelCase" in
  let tokens, _diags = Lexer.lex lexer in
  let ident_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.Ident _ -> true | _ -> false)
      tokens
  in
  check int "four identifiers" 4 (List.length ident_tokens)

let test_integers () =
  let lexer = make_lexer "0 123 999_000" in
  let tokens, _diags = Lexer.lex lexer in
  let int_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.LitInt _ -> true | _ -> false)
      tokens
  in
  check int "three integers" 3 (List.length int_tokens)

let test_floats () =
  let lexer = make_lexer "3.14 0.0 123.456_789" in
  let tokens, _diags = Lexer.lex lexer in
  let float_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.LitFloat _ -> true | _ -> false)
      tokens
  in
  check int "three floats" 3 (List.length float_tokens)

let test_strings () =
  let lexer = make_lexer "\"hello\" \"world\" \"\"" in
  let tokens, _diags = Lexer.lex lexer in
  let string_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.LitText _ -> true | _ -> false)
      tokens
  in
  check int "three strings" 3 (List.length string_tokens)

let test_string_escapes () =
  let lexer = make_lexer "\"\\n\\t\\r\\\\\\\"\\0\"" in
  let tokens, _diags = Lexer.lex lexer in
  match
    List.find_opt
      (fun tok ->
        match tok.Token.kind with Token.LitText _ -> true | _ -> false)
      tokens
  with
  | Some _ -> ()
  | None -> fail "expected string token with escapes"

let test_hex_escapes () =
  let lexer = make_lexer "\"\\x41\\x42\\x43\"" in
  let tokens, _diags = Lexer.lex lexer in
  match
    List.find_opt
      (fun tok ->
        match tok.Token.kind with Token.LitText _ -> true | _ -> false)
      tokens
  with
  | Some _ -> () (* ABC *)
  | None -> fail "expected string with hex escapes"

let test_operators () =
  let lexer = make_lexer "+ - * / = < > <= >= =/= <- -> := ..< ..." in
  let tokens, _diags = Lexer.lex lexer in
  let op_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with
        | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Eq
        | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.EqSlashEq
        | Token.LtMinus | Token.MinusGt | Token.ColonEq | Token.DotDotLt
        | Token.DotDotDot ->
          true
        | _ -> false)
      tokens
  in
  check int "operator count" 15 (List.length op_tokens)

let test_line_comments () =
  let lexer = make_lexer "// hello world\n// another comment" in
  let tokens, _diags = Lexer.lex lexer in
  let comment_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.LineComment _ -> true | _ -> false)
      tokens
  in
  check int "two line comments" 2 (List.length comment_tokens)

let test_block_comments () =
  let lexer = make_lexer "/* hello */ /* world */" in
  let tokens, _diags = Lexer.lex lexer in
  let comment_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.BlockComment _ -> true | _ -> false)
      tokens
  in
  check int "two block comments" 2 (List.length comment_tokens)

let test_nested_block_comments () =
  let lexer = make_lexer "/* outer /* inner */ still outer */" in
  let tokens, _diags = Lexer.lex lexer in
  let comment_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.BlockComment _ -> true | _ -> false)
      tokens
  in
  check int "one nested block comment" 1 (List.length comment_tokens)

let test_unterminated_string () =
  let lexer = make_lexer "\"unterminated" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "has error diagnostic" true (Diagnostic.has_errors diags)

let test_invalid_escape () =
  let lexer = make_lexer "\"\\z\"" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "has error diagnostic" true (Diagnostic.has_errors diags)

let test_unterminated_block_comment () =
  let lexer = make_lexer "/* unterminated" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "has error diagnostic" true (Diagnostic.has_errors diags)

let test_invalid_character () =
  let lexer = make_lexer "§" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "has error diagnostic" true (Diagnostic.has_errors diags)

let test_doc_line_comments () =
  let lexer = make_lexer "/// doc comment\n// regular comment" in
  let tokens, _diags = Lexer.lex lexer in
  let comment_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.LineComment _ -> true | _ -> false)
      tokens
  in
  check int "two line comments" 2 (List.length comment_tokens)

let test_doc_block_comments () =
  let lexer = make_lexer "/** doc comment */ /* regular comment */" in
  let tokens, _diags = Lexer.lex lexer in
  let comment_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with
        | Token.BlockComment { docstyle = true; _ } -> true
        | _ -> false)
      tokens
  in
  check int "one doc block comment" 1 (List.length comment_tokens)

let test_unicode_escapes_braced () =
  let lexer = make_lexer "\"\\u{41}\\u{1F600}\"" in
  let tokens, _diags = Lexer.lex lexer in
  match
    List.find_opt
      (fun tok ->
        match tok.Token.kind with Token.LitText _ -> true | _ -> false)
      tokens
  with
  | Some _ -> ()
  | None -> fail "expected string with unicode escapes"

let test_unicode_escapes_fixed () =
  let lexer = make_lexer "\"\\u0041\\U00000042\"" in
  let tokens, _diags = Lexer.lex lexer in
  match
    List.find_opt
      (fun tok ->
        match tok.Token.kind with Token.LitText _ -> true | _ -> false)
      tokens
  with
  | Some _ -> ()
  | None -> fail "expected string with fixed unicode escapes"

let test_unicode_escape_errors () =
  let lexer = make_lexer "\"\\u{110000}\"" in
  let _tokens, diags = Lexer.lex lexer in
  check
    bool
    "invalid unicode out of range error"
    true
    (Diagnostic.has_errors diags)

let test_unicode_surrogate_error () =
  let lexer = make_lexer "\"\\u{D800}\"" in
  let _tokens, diags = Lexer.lex lexer in
  check
    bool
    "invalid unicode surrogate error"
    true
    (Diagnostic.has_errors diags)

let test_malformed_unicode_escapes () =
  let lexer = make_lexer "\"\\u{\"" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "malformed unicode escape error" true (Diagnostic.has_errors diags)

let test_binary_literals () =
  let lexer = make_lexer "0b1010 0B1111" in
  let tokens, _diags = Lexer.lex lexer in
  let binary_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.LitInt _ -> true | _ -> false)
      tokens
  in
  check int "two binary literals" 2 (List.length binary_tokens)

let test_octal_literals () =
  let lexer = make_lexer "0o777 0O123" in
  let tokens, _diags = Lexer.lex lexer in
  let octal_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.LitInt _ -> true | _ -> false)
      tokens
  in
  check int "two octal literals" 2 (List.length octal_tokens)

let test_hex_literals () =
  let lexer = make_lexer "0xFF 0x123abc" in
  let tokens, _diags = Lexer.lex lexer in
  let hex_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.LitInt _ -> true | _ -> false)
      tokens
  in
  check int "two hex literals" 2 (List.length hex_tokens)

let test_scientific_notation () =
  let lexer = make_lexer "1e10 3.14e-2 2E+5" in
  let tokens, _diags = Lexer.lex lexer in
  let sci_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.LitFloat _ -> true | _ -> false)
      tokens
  in
  check int "three scientific literals" 3 (List.length sci_tokens)

let test_invalid_suffix () =
  let lexer = make_lexer "123xyz" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "invalid suffix error" true (Diagnostic.has_errors diags)

let test_leading_zeros () =
  let lexer = make_lexer "0123" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "leading zeros error" true (Diagnostic.has_errors diags)

let test_empty_suffix () =
  let lexer = make_lexer "123_" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "empty suffix error" true (Diagnostic.has_errors diags)

let test_invalid_hex_escape () =
  let lexer = make_lexer "\"\\xGG\"" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "invalid hex digit error" true (Diagnostic.has_errors diags)

let test_unterminated_hex_escape () =
  let lexer = make_lexer "\"\\x" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "unterminated hex escape error" true (Diagnostic.has_errors diags)

let () =
  run
    "Lexer"
    [
      ( "Lexer"
      , [
          test_case "empty" `Quick test_empty
        ; test_case "whitespace" `Quick test_whitespace
        ; test_case "newlines" `Quick test_newlines
        ; test_case "keywords" `Quick test_keywords
        ; test_case "identifiers" `Quick test_identifiers
        ; test_case "integers" `Quick test_integers
        ; test_case "floats" `Quick test_floats
        ; test_case "strings" `Quick test_strings
        ; test_case "string_escapes" `Quick test_string_escapes
        ; test_case "hex_escapes" `Quick test_hex_escapes
        ; test_case "operators" `Quick test_operators
        ; test_case "line_comments" `Quick test_line_comments
        ; test_case "block_comments" `Quick test_block_comments
        ; test_case "nested_block_comments" `Quick test_nested_block_comments
        ; test_case "unterminated_string" `Quick test_unterminated_string
        ; test_case "invalid_escape" `Quick test_invalid_escape
        ; test_case
            "unterminated_block_comment"
            `Quick
            test_unterminated_block_comment
        ; test_case "invalid_character" `Quick test_invalid_character
        ; test_case "doc_line_comments" `Quick test_doc_line_comments
        ; test_case "doc_block_comments" `Quick test_doc_block_comments
        ; test_case "unicode_escapes_braced" `Quick test_unicode_escapes_braced
        ; test_case "unicode_escapes_fixed" `Quick test_unicode_escapes_fixed
        ; test_case "unicode_escape_errors" `Quick test_unicode_escape_errors
        ; test_case
            "unicode_surrogate_error"
            `Quick
            test_unicode_surrogate_error
        ; test_case "invalid_hex_escape" `Quick test_invalid_hex_escape
        ; test_case
            "unterminated_hex_escape"
            `Quick
            test_unterminated_hex_escape
        ; test_case
            "malformed_unicode_escapes"
            `Quick
            test_malformed_unicode_escapes
        ; test_case "binary_literals" `Quick test_binary_literals
        ; test_case "octal_literals" `Quick test_octal_literals
        ; test_case "hex_literals" `Quick test_hex_literals
        ; test_case "scientific_notation" `Quick test_scientific_notation
        ; test_case "invalid_suffix" `Quick test_invalid_suffix
        ; test_case "leading_zeros" `Quick test_leading_zeros
        ; test_case "empty_suffix" `Quick test_empty_suffix
        ] )
    ]
