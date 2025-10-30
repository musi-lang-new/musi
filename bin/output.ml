let status_prefix status = Ansi.bold (Printf.sprintf "%12s" status)

let compiling file =
  Printf.printf "%s %s\n" (status_prefix (Ansi.bold_green "Compiling")) file;
  flush stdout

let finished profile target =
  Printf.printf
    "%s %s target -> %s\n"
    (status_prefix (Ansi.bold_green "Finished"))
    profile
    target;
  flush stdout

let checking file =
  Printf.printf "%s %s\n" (status_prefix (Ansi.bold_green "Checking")) file;
  flush stdout

let error msg =
  Printf.eprintf "%s %s\n" (status_prefix (Ansi.bold_red "Error")) msg;
  flush stderr

let warning msg =
  Printf.eprintf "%s %s\n" (status_prefix (Ansi.bold_yellow "Warning")) msg;
  flush stderr

let running file =
  Printf.printf "%s %s\n" (status_prefix (Ansi.bold_green "Running")) file;
  flush stdout
