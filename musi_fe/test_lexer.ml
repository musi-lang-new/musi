open Alcotest

let make_lexer source =
  let interner = Interner.create () in
  Lexer.make 0 source interner

let basic_tokens () =
  let lexer = make_lexer "" in
  let tokens, _diags = Lexer.lex lexer in
  check int "empty source has only EOF" 1 (List.length tokens);

  let lexer = make_lexer "   \t\r  " in
  let tokens, _diags = Lexer.lex lexer in
  check int "whitespace tokens plus EOF" 2 (List.length tokens);

  let lexer = make_lexer "\n\n\n" in
  let tokens, _diags = Lexer.lex lexer in
  check int "three newlines plus EOF" 4 (List.length tokens)

let keywords_and_identifiers () =
  let lexer = make_lexer "proc if else while for" in
  let tokens, _diags = Lexer.lex lexer in
  let keyword_count =
    List.fold_left
      (fun acc tok ->
        match tok.Token.kind with
        | Token.KwProc | Token.KwIf | Token.KwElse | Token.KwWhile | Token.KwFor
          ->
          acc + 1
        | _ -> acc)
      0
      tokens
  in
  check int "five keywords" 5 keyword_count;

  let lexer = make_lexer "hello world_123 _private CamelCase" in
  let tokens, _diags = Lexer.lex lexer in
  let ident_count =
    List.fold_left
      (fun acc tok ->
        match tok.Token.kind with Token.Ident _ -> acc + 1 | _ -> acc)
      0
      tokens
  in
  check int "four identifiers" 4 ident_count

let numeric_literals () =
  let count_tokens lexer kind_pred =
    let tokens, _diags = Lexer.lex lexer in
    List.fold_left
      (fun acc tok -> if kind_pred tok.Token.kind then acc + 1 else acc)
      0
      tokens
  in

  let lexer = make_lexer "0 123 999_000" in
  let int_count =
    count_tokens lexer (function Token.IntLit _ -> true | _ -> false)
  in
  check int "three integers" 3 int_count;

  let lexer = make_lexer "3.14 0.0 123.456_789" in
  let float_count =
    count_tokens lexer (function Token.FloatLit _ -> true | _ -> false)
  in
  check int "three floats" 3 float_count

let string_literals () =
  let has_string_token lexer =
    let tokens, _diags = Lexer.lex lexer in
    List.exists
      (fun tok ->
        match tok.Token.kind with Token.TextLit _ -> true | _ -> false)
      tokens
  in

  let lexer = make_lexer "\"hello\" \"world\" \"\"" in
  let tokens, _diags = Lexer.lex lexer in
  let string_count =
    List.fold_left
      (fun acc tok ->
        match tok.Token.kind with Token.TextLit _ -> acc + 1 | _ -> acc)
      0
      tokens
  in
  check int "three strings" 3 string_count;

  let lexer = make_lexer "\"\\n\\t\\r\\\\\\\"\\0\"" in
  check bool "string with escapes" true (has_string_token lexer);

  let lexer = make_lexer "\"\\x41\\x42\\x43\"" in
  check bool "string with hex escapes" true (has_string_token lexer)

let test_operators () =
  let lexer = make_lexer "+ - * / = < > <= >= =/= <- -> := ..< .." in
  let tokens, _diags = Lexer.lex lexer in
  let op_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with
        | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Eq
        | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.EqSlashEq
        | Token.LtMinus | Token.MinusGt | Token.ColonEq | Token.DotDotLt
        | Token.DotDot ->
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
        match tok.Token.kind with Token.TextLit _ -> true | _ -> false)
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
        match tok.Token.kind with Token.TextLit _ -> true | _ -> false)
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
        match tok.Token.kind with Token.IntLit _ -> true | _ -> false)
      tokens
  in
  check int "two binary literals" 2 (List.length binary_tokens)

let test_octal_literals () =
  let lexer = make_lexer "0o777 0O123" in
  let tokens, _diags = Lexer.lex lexer in
  let octal_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.IntLit _ -> true | _ -> false)
      tokens
  in
  check int "two octal literals" 2 (List.length octal_tokens)

let test_hex_literals () =
  let lexer = make_lexer "0xFF 0x123abc" in
  let tokens, _diags = Lexer.lex lexer in
  let hex_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.IntLit _ -> true | _ -> false)
      tokens
  in
  check int "two hex literals" 2 (List.length hex_tokens)

let test_scientific_notation () =
  let lexer = make_lexer "1e10 3.14e-2 2E+5" in
  let tokens, _diags = Lexer.lex lexer in
  let sci_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with Token.FloatLit _ -> true | _ -> false)
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

let test_template_literals () =
  let lexer = make_lexer "`hello world`" in
  let tokens, _diags = Lexer.lex lexer in
  let template_tokens =
    List.filter
      (fun tok ->
        match tok.Token.kind with
        | Token.NoSubstTemplateLit _ -> true
        | _ -> false)
      tokens
  in
  check int "one template literal" 1 (List.length template_tokens)

let test_numeric_overflow () =
  let lexer = make_lexer "999999999999999999999999999999" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "numeric overflow warning" true (diags.warnings > 0)

let test_consecutive_underscores () =
  let lexer = make_lexer "1__2" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "consecutive underscores warning" true (diags.warnings > 0)

let test_confusing_octal () =
  let lexer = make_lexer "0O123" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "confusing octal warning" true (diags.warnings > 0)

let test_mixed_separators () =
  let lexer = make_lexer "123_456.789" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "mixed separators warning" true (diags.warnings > 0)

let test_invalid_hex_escape () =
  let lexer = make_lexer "\"\\xGG\"" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "invalid hex digit error" true (Diagnostic.has_errors diags)

let test_unterminated_hex_escape () =
  let lexer = make_lexer "\"\\x" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "unterminated hex escape error" true (Diagnostic.has_errors diags)

let test_unknown_escape_sequences () =
  let lexer = make_lexer "\"\\a\\b\\c\"" in
  let _tokens, diags = Lexer.lex lexer in
  check bool "unknown escape sequences error" true (Diagnostic.has_errors diags)

let () =
  run
    "Lexer"
    [
      ( "Basic Tokens"
      , [
          test_case "basic_tokens" `Quick basic_tokens
        ; test_case "keywords_and_identifiers" `Quick keywords_and_identifiers
        ; test_case "operators" `Quick test_operators
        ] )
    ; ( "Numeric Literals"
      , [
          test_case "numeric_literals" `Quick numeric_literals
        ; test_case "binary_literals" `Quick test_binary_literals
        ; test_case "octal_literals" `Quick test_octal_literals
        ; test_case "hex_literals" `Quick test_hex_literals
        ; test_case "scientific_notation" `Quick test_scientific_notation
        ] )
    ; ( "String Literals"
      , [
          test_case "string_literals" `Quick string_literals
        ; test_case "template_literals" `Quick test_template_literals
        ] )
    ; ( "Comments"
      , [
          test_case "line_comments" `Quick test_line_comments
        ; test_case "block_comments" `Quick test_block_comments
        ; test_case "nested_block_comments" `Quick test_nested_block_comments
        ; test_case "doc_line_comments" `Quick test_doc_line_comments
        ; test_case "doc_block_comments" `Quick test_doc_block_comments
        ] )
    ; ( "Unicode Support"
      , [
          test_case "unicode_escapes_braced" `Quick test_unicode_escapes_braced
        ; test_case "unicode_escapes_fixed" `Quick test_unicode_escapes_fixed
        ; test_case "unicode_escape_errors" `Quick test_unicode_escape_errors
        ; test_case
            "unicode_surrogate_error"
            `Quick
            test_unicode_surrogate_error
        ; test_case
            "malformed_unicode_escapes"
            `Quick
            test_malformed_unicode_escapes
        ] )
    ; ( "Error Cases"
      , [
          test_case "unterminated_string" `Quick test_unterminated_string
        ; test_case "invalid_escape" `Quick test_invalid_escape
        ; test_case
            "unterminated_block_comment"
            `Quick
            test_unterminated_block_comment
        ; test_case "invalid_character" `Quick test_invalid_character
        ; test_case "invalid_hex_escape" `Quick test_invalid_hex_escape
        ; test_case
            "unterminated_hex_escape"
            `Quick
            test_unterminated_hex_escape
        ; test_case
            "unknown_escape_sequences"
            `Quick
            test_unknown_escape_sequences
        ; test_case "invalid_suffix" `Quick test_invalid_suffix
        ; test_case "leading_zeros" `Quick test_leading_zeros
        ; test_case "empty_suffix" `Quick test_empty_suffix
        ] )
    ; ( "Warnings"
      , [
          test_case "numeric_overflow" `Quick test_numeric_overflow
        ; test_case
            "consecutive_underscores"
            `Quick
            test_consecutive_underscores
        ; test_case "confusing_octal" `Quick test_confusing_octal
        ; test_case "mixed_separators" `Quick test_mixed_separators
        ] )
    ]
