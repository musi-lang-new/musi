open Alcotest

module Interner = Musi_shared.Interner

let test_create () =
  let interner = Interner.create () in
  let sym1 = Interner.intern interner "test" in
  let sym2 = Interner.intern interner "test" in
  check bool "same string gives same symbol" true (Interner.equals sym1 sym2)

let test_intern_single () =
  let interner = Interner.create () in
  let sym = Interner.intern interner "hello" in
  check
    string
    "resolve returns original string"
    "hello"
    (Interner.resolve interner sym)

let test_intern_multiple () =
  let interner = Interner.create () in
  let sym1 = Interner.intern interner "hello" in
  let sym2 = Interner.intern interner "world" in
  check bool "different symbols not equal" false (Interner.equals sym1 sym2);
  check string "resolve sym1" "hello" (Interner.resolve interner sym1);
  check string "resolve sym2" "world" (Interner.resolve interner sym2)

let test_intern_duplicate () =
  let interner = Interner.create () in
  let sym1 = Interner.intern interner "hello" in
  let sym2 = Interner.intern interner "hello" in
  check bool "duplicate returns same symbol" true (Interner.equals sym1 sym2);
  let sym3 = Interner.intern interner "hello" in
  check bool "third intern returns same symbol" true (Interner.equals sym1 sym3)

let test_intern_empty_string () =
  let interner = Interner.create () in
  let sym = Interner.intern interner "" in
  check
    string
    "empty string resolves correctly"
    ""
    (Interner.resolve interner sym)

let test_intern_unicode () =
  let interner = Interner.create () in
  let sym = Interner.intern interner "🦀" in
  check string "unicode resolves correctly" "🦀" (Interner.resolve interner sym)

let test_compare () =
  let interner = Interner.create () in
  let sym1 = Interner.intern interner "a" in
  let sym2 = Interner.intern interner "b" in
  let sym3 = Interner.intern interner "a" in
  check int "compare equal symbols" 0 (Interner.compare sym1 sym3);
  check bool "compare different symbols" true (Interner.compare sym1 sym2 <> 0)

let test_equals () =
  let interner = Interner.create () in
  let sym1 = Interner.intern interner "test" in
  let sym2 = Interner.intern interner "test" in
  let sym3 = Interner.intern interner "other" in
  check bool "equals same symbols" true (Interner.equals sym1 sym2);
  check bool "equals different symbols" false (Interner.equals sym1 sym3)

let () =
  run
    "Intern"
    [
      ( "Intern"
      , [
          test_case "create" `Quick test_create
        ; test_case "intern_single" `Quick test_intern_single
        ; test_case "intern_multiple" `Quick test_intern_multiple
        ; test_case "intern_duplicate" `Quick test_intern_duplicate
        ; test_case "intern_empty_string" `Quick test_intern_empty_string
        ; test_case "intern_unicode" `Quick test_intern_unicode
        ; test_case "compare" `Quick test_compare
        ; test_case "equals" `Quick test_equals
        ] )
    ]
