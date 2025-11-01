open Alcotest

let test_builtin_writeln () =
  let source =
    "const __builtin_writeln := unsafe extern \"intrinsic\" proc (s: Text); \
     __builtin_writeln(\"test\")"
  in
  let exit_code = Helpers.compile_and_run source in
  check int "builtin writeln succeeds" 0 exit_code

let test_unregistered_extern () =
  let source =
    "const f := unsafe extern \"intrinsic\" proc (s: Text); f(\"test\")"
  in
  let exit_code = Helpers.compile_and_run source in
  check int "unregistered extern fails" 1 exit_code

let () =
  run
    "Integration"
    [
      ( "extern-intrinsics"
      , [
          test_case "builtin-writeln" `Quick test_builtin_writeln
        ; test_case "unregistered-extern" `Quick test_unregistered_extern
        ] )
    ]
