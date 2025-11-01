(** Links modules by resolving imports and tracking exports. *)

type module_info = {
    path : string
  ; ast : Node.program
  ; exports : Interner.symbol list
}

type t = {
    interner : Interner.t
  ; search_paths : string list
  ; loaded : (string, module_info) Hashtbl.t
  ; mutable loading : string list
}

let create interner search_paths =
  { interner; search_paths; loaded = Hashtbl.create 16; loading = [] }

let resolve_path t import_path =
  let try_path base =
    let full_path = Filename.concat base import_path in
    if Sys.file_exists full_path then Some full_path else None
  in
  List.find_map try_path t.search_paths

let extract_exports ast =
  let exports = ref [] in
  List.iter
    (fun (node : Node.node) ->
      match node.kind with
      | Node.ExprExport { kind = Named { items }; _ } ->
        List.iter
          (fun (item : Node.import_export_item) ->
            exports := item.name :: !exports)
          items
      | _ -> ())
    ast;
  !exports

let rec resolve_imports t ast =
  List.fold_left
    (fun acc (node : Node.node) ->
      match (acc, node.kind) with
      | Ok (), Node.ExprImport { source; _ } ->
        let source_str = Interner.resolve t.interner source in
        load_module t source_str |> Result.map (fun _ -> ())
      | _ -> acc)
    (Ok ())
    ast

and load_module t import_path =
  match Hashtbl.find_opt t.loaded import_path with
  | Some info -> Ok info
  | None -> (
    if List.mem import_path t.loading then
      Error (Printf.sprintf "circular import '%s'" import_path)
    else
      match resolve_path t import_path with
      | None -> Error (Printf.sprintf "module '%s' not found" import_path)
      | Some file_path -> (
        t.loading <- import_path :: t.loading;
        let ic = open_in file_path in
        let source = really_input_string ic (in_channel_length ic) in
        close_in ic;
        let lexer = Lexer.make 0 source t.interner in
        let tokens, lex_diags = Lexer.lex lexer in
        if Diagnostic.has_errors lex_diags then (
          t.loading <- List.filter (( <> ) import_path) t.loading;
          Error (Printf.sprintf "errors in module '%s'" import_path))
        else
          let ast, parse_diags = Parser.parse_program tokens t.interner in
          if Diagnostic.has_errors parse_diags then (
            t.loading <- List.filter (( <> ) import_path) t.loading;
            Error (Printf.sprintf "errors in module '%s'" import_path))
          else
            match resolve_imports t ast with
            | Error msg ->
              t.loading <- List.filter (( <> ) import_path) t.loading;
              Error msg
            | Ok () ->
              let exports = extract_exports ast in
              let info = { path = file_path; ast; exports } in
              Hashtbl.add t.loaded import_path info;
              t.loading <- List.filter (( <> ) import_path) t.loading;
              Ok info))

let all_modules t = Hashtbl.fold (fun _ info acc -> info :: acc) t.loaded []
