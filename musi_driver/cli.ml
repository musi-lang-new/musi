type command =
  | Compile of { input : string; output : string option }
  | Check of { input : string }
  | Run of { input : string }
  | Help
  | Version

let parse_args () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: "compile" :: input :: rest ->
    let output =
      match rest with
      | "-o" :: out :: _ | "--output" :: out :: _ -> Some out
      | _ -> None
    in
    Compile { input; output }
  | _ :: "check" :: input :: _ -> Check { input }
  | _ :: "run" :: input :: _ -> Run { input }
  | _ :: ("help" | "--help" | "-h") :: _ -> Help
  | _ :: ("version" | "--version" | "-V") :: _ -> Version
  | _ :: input :: rest when String.ends_with ~suffix:".ms" input ->
    let output =
      match rest with
      | "-o" :: out :: _ | "--output" :: out :: _ -> Some out
      | _ -> None
    in
    Compile { input; output }
  | _ -> Help
