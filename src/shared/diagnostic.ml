type severity = Error | Warning | Note

type t = {
    severity : severity
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

type diagnostic_bag = { diags : t list; errors : int; warnings : int }

let make severity message span = { severity; message; span; notes = [] }
let error message span = make Error message span
let warning message span = make Warning message span
let note message span = make Note message span
let with_note t message span = { t with notes = (message, span) :: t.notes }
let empty_bag = { diags = []; errors = 0; warnings = 0 }
let is_empty bag = bag.diags = []
let has_errors bag = bag.errors > 0

let add bag diag =
  let errors, warnings =
    match diag.severity with
    | Error -> (bag.errors + 1, bag.warnings)
    | Warning -> (bag.errors, bag.warnings + 1)
    | Note -> (bag.errors, bag.warnings)
  in
  { diags = diag :: bag.diags; errors; warnings }

let to_list bag = List.rev bag.diags

let merge bags =
  List.fold_left
    (fun acc bag ->
      {
        diags = bag.diags @ acc.diags
      ; errors = acc.errors + bag.errors
      ; warnings = acc.warnings + bag.warnings
      })
    empty_bag
    bags

let emit ppf diag files =
  let severity_str =
    match diag.severity with
    | Error -> "error"
    | Warning -> "warning"
    | Note -> "note"
  in
  match Source.get_file files (Span.file diag.span) with
  | None -> Format.fprintf ppf "%s: %s@." severity_str diag.message
  | Some file ->
    let line, col = Source.line_col file (Span.start diag.span) in
    Format.fprintf
      ppf
      "%s:%d:%d: %s: %s@."
      (Source.path file)
      line
      col
      severity_str
      diag.message;
    (match Source.line_text file line with
    | None -> ()
    | Some text ->
      Format.fprintf ppf " %d | %s@." line text;
      let end_line, end_col = Source.line_col file (Span.end_ diag.span) in
      let len =
        if line = end_line then max 1 (end_col - col)
        else max 1 (String.length text - col + 1)
      in
      Format.fprintf
        ppf
        " %s | %s%s@."
        (String.make (String.length (string_of_int line)) ' ')
        (String.make (col - 1) ' ')
        (String.make len '^'));
    List.iter
      (fun (msg, span) ->
        match Source.get_file files (Span.file span) with
        | None -> Format.fprintf ppf "note: %s@." msg
        | Some f ->
          let l, c = Source.line_col f (Span.start span) in
          Format.fprintf ppf "%s:%d:%d: note: %s@." (Source.path f) l c msg)
      (List.rev diag.notes)

let emit_all ppf bag files = List.iter (fun d -> emit ppf d files) (to_list bag)
