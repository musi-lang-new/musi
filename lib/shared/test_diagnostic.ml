open Alcotest

let test_empty_bag () =
  let bag = Diagnostic.empty_bag in
  check int "empty bag has 0 errors" 0 bag.errors;
  check int "empty bag has 0 warnings" 0 bag.warnings;
  check int "empty bag has 0 diags" 0 (List.length bag.diags);
  check bool "empty bag has no errors" false (Diagnostic.has_errors bag)

let test_add_error () =
  let bag = Diagnostic.empty_bag in
  let span = Span.make 0 0 10 in
  let diag = Diagnostic.error "test error" span in
  let new_bag = Diagnostic.add bag diag in
  check int "bag has 1 error" 1 new_bag.errors;
  check int "bag has 0 warnings" 0 new_bag.warnings;
  check int "bag has 1 diag" 1 (List.length new_bag.diags);
  check bool "bag has errors" true (Diagnostic.has_errors new_bag)

let test_add_warning () =
  let bag = Diagnostic.empty_bag in
  let span = Span.make 0 0 10 in
  let diag = Diagnostic.warning "test warning" span in
  let new_bag = Diagnostic.add bag diag in
  check int "bag has 0 errors" 0 new_bag.errors;
  check int "bag has 1 warning" 1 new_bag.warnings;
  check int "bag has 1 diag" 1 (List.length new_bag.diags);
  check bool "bag has no errors" false (Diagnostic.has_errors new_bag)

let () =
  run
    "Diagnostic"
    [
      ("empty_bag", [ test_case "empty" `Quick test_empty_bag ])
    ; ("add_error", [ test_case "error" `Quick test_add_error ])
    ; ("add_warning", [ test_case "warning" `Quick test_add_warning ])
    ]
