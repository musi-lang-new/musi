open Alcotest

let test_import_exported_symbol () =
  let package_source = "const x := 42;\nexport { x };" in
  let main_source = "import { x } from \"lib.ms\";\nx" in
  let exit_code =
    Helpers.compile_and_run_with_package "lib.ms" package_source main_source
  in
  check int "import succeeds" 0 exit_code

let test_circular_import () =
  let a_source = "import { b } from \"b.ms\";\nconst a := 1;\nexport { a };" in
  let b_source = "import { a } from \"a.ms\";\nconst b := 2;\nexport { b };" in
  let exit_code =
    Helpers.compile_and_run_with_packages
      [ ("a.ms", a_source); ("b.ms", b_source) ]
      "import { a } from \"a.ms\";\na"
  in
  check int "circular import fails" 1 exit_code

let () =
  run
    "Packages"
    [
      ( "imports"
      , [
          test_case "import-exported-symbol" `Quick test_import_exported_symbol
        ; test_case "circular-import" `Quick test_circular_import
        ] )
    ]
