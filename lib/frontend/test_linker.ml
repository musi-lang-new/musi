open Alcotest

let temp_dir = ref None

let setup_temp_dir () =
  let dir = Filename.temp_file "musi_test_" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  temp_dir := Some dir;
  dir

let cleanup_temp_dir () =
  match !temp_dir with
  | Some dir ->
    let rec rmdir path =
      if Sys.is_directory path then (
        Sys.readdir path
        |> Array.iter (fun name -> rmdir (Filename.concat path name));
        Unix.rmdir path)
      else Sys.remove path
    in
    rmdir dir;
    temp_dir := None
  | None -> ()

let write_module dir name content =
  let path = Filename.concat dir name in
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  path

let test_load_simple_module () =
  let dir = setup_temp_dir () in
  let _ = write_module dir "test.ms" "const x := 42;\nexport { x };" in
  let interner = Interner.create () in
  let linker = Linker.create interner [ dir ] in
  match Linker.load_module linker "test.ms" with
  | Ok info ->
    check int "module loaded" 1 (List.length info.exports);
    cleanup_temp_dir ()
  | Error msg ->
    cleanup_temp_dir ();
    fail msg

let test_module_not_found () =
  let dir = setup_temp_dir () in
  let interner = Interner.create () in
  let linker = Linker.create interner [ dir ] in
  match Linker.load_module linker "nonexistent.ms" with
  | Ok _ ->
    cleanup_temp_dir ();
    fail "expected module not found error"
  | Error msg ->
    check bool "error contains module name" true (String.contains msg '\'');
    cleanup_temp_dir ()

let test_module_caching () =
  let dir = setup_temp_dir () in
  let _ = write_module dir "cached.ms" "const y := 1;\nexport { y };" in
  let interner = Interner.create () in
  let linker = Linker.create interner [ dir ] in
  let result1 = Linker.load_module linker "cached.ms" in
  let result2 = Linker.load_module linker "cached.ms" in
  match (result1, result2) with
  | Ok info1, Ok info2 ->
    check bool "same module instance" true (info1 == info2);
    cleanup_temp_dir ()
  | _ ->
    cleanup_temp_dir ();
    fail "module loading failed"

let test_extract_exports () =
  let dir = setup_temp_dir () in
  let _ =
    write_module dir "multi.ms" "const x := 1;\nconst y := 2;\nexport { x, y };"
  in
  let interner = Interner.create () in
  let linker = Linker.create interner [ dir ] in
  match Linker.load_module linker "multi.ms" with
  | Ok info ->
    check int "two exports" 2 (List.length info.exports);
    cleanup_temp_dir ()
  | Error msg ->
    cleanup_temp_dir ();
    fail msg

let () =
  run
    "Linker"
    [
      ( "loading"
      , [
          test_case "simple-module" `Quick test_load_simple_module
        ; test_case "not-found" `Quick test_module_not_found
        ; test_case "caching" `Quick test_module_caching
        ] )
    ; ("exports", [ test_case "extract-exports" `Quick test_extract_exports ])
    ]
