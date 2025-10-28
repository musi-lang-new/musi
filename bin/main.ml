let () =
  let exit_code =
    try
      match Cli.parse_args () with
      | Cli.Compile { input; output } -> Commands.compile input output
      | Cli.Check { input } -> Commands.check input
      | Cli.Run { input } -> Commands.run input
      | Cli.Help -> Commands.help ()
      | Cli.Version -> Commands.version ()
    with
    | Sys_error msg ->
      Output.error msg;
      1
    | exn ->
      Output.error (Printexc.to_string exn);
      1
  in
  exit exit_code
