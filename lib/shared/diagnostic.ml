type severity = Error | Warning | Note
type fixit = { span : Span.t; replacement : string }

type t = {
    severity : severity
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
  ; fixits : fixit list
}

type diagnostic_bag = { diags : t list; errors : int; warnings : int }

let make severity message span =
  { severity; message; span; notes = []; fixits = [] }

let error message span = make Error message span
let warning message span = make Warning message span
let note message span = make Note message span
let with_note t message span = { t with notes = (message, span) :: t.notes }
let with_fixit t fixit = { t with fixits = fixit :: t.fixits }
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

let format_severity_header = function
  | Error -> Ansi.bold_red "error:"
  | Warning -> Ansi.bold_yellow "warning:"
  | Note -> Ansi.bold_cyan "note:"

let caret_color_for_severity = function
  | Error -> Ansi.bold_red
  | Warning -> Ansi.bold_yellow
  | Note -> Ansi.bold_cyan

let format_source_line ppf line_num text =
  Format.fprintf
    ppf
    " %s %s %s@."
    (Ansi.bold (string_of_int line_num))
    (Ansi.bold "|")
    text

let format_caret ppf line_num col len color_fn =
  Format.fprintf
    ppf
    " %s %s %s%s@."
    (Ansi.bold (String.make (String.length (string_of_int line_num)) ' '))
    (Ansi.bold "|")
    (String.make (col - 1) ' ')
    (color_fn (String.make len '^'))

let format_fixit ppf line_num col replacement =
  Format.fprintf
    ppf
    " %s %s %s%s@."
    (Ansi.bold (String.make (String.length (string_of_int line_num)) ' '))
    (Ansi.bold "|")
    (String.make (col - 1) ' ')
    (Ansi.bold_green replacement)

let emit ppf diag files =
  let severity_header = format_severity_header diag.severity in
  match Source.get_file files (Span.file diag.span) with
  | None -> Format.fprintf ppf "%s %s@." severity_header diag.message
  | Some file ->
    let line, col = Source.line_col file (Span.start diag.span) in
    Format.fprintf
      ppf
      "%s:%d:%d: %s %s@."
      (Ansi.bold (Source.path file))
      line
      col
      severity_header
      (Ansi.bold diag.message);
    (match Source.line_text file line with
    | None -> ()
    | Some text ->
      format_source_line ppf line text;
      let end_line, end_col = Source.line_col file (Span.end_ diag.span) in
      let len =
        if line = end_line then max 1 (end_col - col)
        else max 1 (String.length text - col + 1)
      in
      let caret_color =
        if diag.fixits = [] then caret_color_for_severity diag.severity
        else Ansi.bold_green
      in
      format_caret ppf line col len caret_color;
      List.iter
        (fun fixit -> format_fixit ppf line col fixit.replacement)
        (List.rev diag.fixits));
    List.iter
      (fun (msg, span) ->
        let note_header = Ansi.bold_cyan "note:" in
        match Source.get_file files (Span.file span) with
        | None -> Format.fprintf ppf "%s %s@." note_header msg
        | Some f ->
          let l, c = Source.line_col f (Span.start span) in
          Format.fprintf
            ppf
            "%s:%d:%d: %s %s@."
            (Ansi.bold (Source.path f))
            l
            c
            note_header
            msg)
      (List.rev diag.notes)

let emit_all ppf bag files = List.iter (fun d -> emit ppf d files) (to_list bag)
