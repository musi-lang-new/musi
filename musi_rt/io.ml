let __builtin_writeln runtime_error = function
  | [ value ] ->
    let output =
      match value with
      | Value.Unit -> "()"
      | Value.Bool b -> if b then "true" else "false"
      | Value.Int i -> string_of_int i
      | Value.Nat n -> string_of_int n
      | Value.HeapRef _ -> Value.text_content value
    in
    print_endline output;
    Value.Unit
  | _ -> raise (runtime_error "procedure 'writeln' expects 1 argument")

let __builtin_write runtime_error = function
  | [ value ] ->
    let output =
      match value with
      | Value.Unit -> "()"
      | Value.Bool b -> if b then "true" else "false"
      | Value.Int i -> string_of_int i
      | Value.Nat n -> string_of_int n
      | Value.HeapRef _ -> Value.text_content value
    in
    print_string output;
    Value.Unit
  | _ -> raise (runtime_error "procedure 'write' expects 1 argument")

let register runtime_error register_fn =
  register_fn "writeln" (__builtin_writeln runtime_error);
  register_fn "write" (__builtin_write runtime_error)
