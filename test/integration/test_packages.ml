open Alcotest

let test_import_exported_symbol () =
  let package_source = "const x := 42;\nexport { x };" in
  let main_source = "import { x } from \"lib.ms\";\nx" in
  let exit_code =
    Helpers.compile_and_run_with_module "lib.ms" package_source main_source
  in
  check int "import succeeds" 0 exit_code

let () =
  run
    "Packages"
    [
      ( "imports"
      , [
          test_case "import-exported-symbol" `Quick test_import_exported_symbol
        ] )
    ]
