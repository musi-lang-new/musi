let green s = "\027[32m" ^ s ^ "\027[0m"
let red s = "\027[31m" ^ s ^ "\027[0m"
let yellow s = "\027[33m" ^ s ^ "\027[0m"
let bold s = "\027[1m" ^ s ^ "\027[0m"
let status_prefix status = bold (Printf.sprintf "%12s" status)

let compiling file =
  Printf.printf "%s %s\n" (status_prefix (green "Compiling")) file;
  flush stdout

let finished target =
  Printf.printf "%s %s\n" (status_prefix (green "Finished")) target;
  flush stdout

let checking file =
  Printf.printf "%s %s\n" (status_prefix (green "Checking")) file;
  flush stdout

let error msg =
  Printf.eprintf "%s %s\n" (status_prefix (red "Error")) msg;
  flush stderr

let warning msg =
  Printf.eprintf "%s %s\n" (status_prefix (yellow "Warning")) msg;
  flush stderr

let running file =
  Printf.printf "%s %s\n" (status_prefix (green "Running")) file;
  flush stdout
