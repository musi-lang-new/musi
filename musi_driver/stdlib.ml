let stdlib_files = [ "stdlib/io.ms"; "stdlib/prelude.ms" ]

let find_stdlib_root () =
  let rec search_up dir depth =
    if depth > 5 then None
    else
      let stdlib_path = Filename.concat dir "stdlib" in
      if Sys.file_exists stdlib_path && Sys.is_directory stdlib_path then
        Some dir
      else
        let parent = Filename.dirname dir in
        if parent = dir then None else search_up parent (depth + 1)
  in
  search_up (Sys.getcwd ()) 0

let parse_stdlib_file interner filepath =
  try
    let ic = open_in filepath in
    let source = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let file_id = 0 in
    let lexer = Lexer.make file_id source interner in
    let tokens, _ = Lexer.lex lexer in
    let ast, _ = Parser.parse_program tokens interner in
    Some ast
  with _ -> None

let load_stdlib interner =
  match find_stdlib_root () with
  | Some root ->
    let full_paths = List.map (Filename.concat root) stdlib_files in
    let results = List.filter_map (parse_stdlib_file interner) full_paths in
    List.flatten results
  | None ->
    Printf.eprintf "'stdlib' directory not found\n";
    []
