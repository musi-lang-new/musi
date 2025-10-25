type file = {
    id : Span.file_id
  ; path : string
  ; source : string
  ; lines : int array
}

type t = file list

let compute_lines source =
  let rec loop i acc =
    if i >= String.length source then Array.of_list (List.rev acc)
    else if source.[i] = '\n' then loop (i + 1) ((i + 1) :: acc)
    else loop (i + 1) acc
  in
  loop 0 [ 0 ]

let empty = []

let add_file files path source =
  let id = List.length files in
  let file = { id; path; source; lines = compute_lines source } in
  (id, file :: files)

let get_file files id = List.find_opt (fun f -> f.id = id) files

let line_col file offset =
  let rec search lo hi =
    if lo > hi then lo - 1
    else
      let mid = (lo + hi) / 2 in
      if file.lines.(mid) <= offset then search (mid + 1) hi
      else search lo (mid - 1)
  in
  let line = max 0 (search 0 (Array.length file.lines - 1)) in
  let col = offset - file.lines.(line) in
  (line + 1, col + 1)

let line_text file line =
  if line < 1 || line > Array.length file.lines then None
  else
    let start = file.lines.(line - 1) in
    let end_ =
      if line < Array.length file.lines then file.lines.(line)
      else String.length file.source
    in
    Some (String.sub file.source start (end_ - start) |> String.trim)

let path file = file.path

type digest = string

let compute_digest text = Digest.(string text |> to_hex)
