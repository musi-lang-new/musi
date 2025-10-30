open Instr

exception Runtime_error of string

type frame = {
    return_ip : int
  ; return_proc : int
  ; base_pointer : int [@warning "-69"]
  ; locals : Value.value array
}

type t = {
    mutable stack : Value.value array [@warning "-69"]
  ; mutable sp : int
  ; mutable frames : frame list
  ; mutable ip : int
  ; mutable current_proc : int
  ; constants : constant array
  ; procs : proc_def array
  ; mutable alloc_count : int
  ; builtins : Builtins.t
}

let create (program : Instr.program) =
  {
    stack = Array.make 1024 Value.Unit
  ; sp = 0
  ; frames = []
  ; ip = 0
  ; current_proc = 0
  ; constants = program.constants
  ; procs = program.procs
  ; alloc_count = 0
  ; builtins = Builtins.prep_stdlib ()
  }

let register_builtin vm name fn = Builtins.register vm.builtins name fn

let push vm value =
  if vm.sp >= Array.length vm.stack then raise (Runtime_error "stack overflow");
  Memory.retain value;
  vm.stack.(vm.sp) <- value;
  vm.sp <- vm.sp + 1

let pop vm =
  if vm.sp <= 0 then raise (Runtime_error "stack underflow");
  vm.sp <- vm.sp - 1;
  let value = vm.stack.(vm.sp) in
  vm.stack.(vm.sp) <- Value.Unit;
  value

let peek vm =
  if vm.sp <= 0 then raise (Runtime_error "stack underflow");
  vm.stack.(vm.sp - 1)

let trigger_gc vm =
  let roots = Array.to_list (Array.sub vm.stack 0 vm.sp) in
  let frame_locals =
    List.concat_map (fun frame -> Array.to_list frame.locals) vm.frames
  in
  Gc.collect (roots @ frame_locals);
  vm.alloc_count <- 0

let check_gc vm =
  vm.alloc_count <- vm.alloc_count + 1;
  if vm.alloc_count >= 100 then trigger_gc vm

let call_extern vm extern_name param_count =
  match Builtins.lookup vm.builtins extern_name with
  | Some fn ->
    let args = Array.init param_count (fun _ -> pop vm) in
    Array.iter Memory.retain args;
    let args_list = List.rev (Array.to_list args) in
    let result = fn args_list in
    Array.iter Memory.release args;
    push vm result
  | None ->
    raise
      (Runtime_error (Printf.sprintf "unresolved external '%s'" extern_name))

(* Builtin implementations *)
let decode_i4 code offset =
  let b0 = Char.code code.[offset] in
  let b1 = Char.code code.[offset + 1] in
  let b2 = Char.code code.[offset + 2] in
  let b3 = Char.code code.[offset + 3] in
  Int32.to_int
    (Int32.logor
       (Int32.logor (Int32.of_int b0) (Int32.shift_left (Int32.of_int b1) 8))
       (Int32.logor
          (Int32.shift_left (Int32.of_int b2) 16)
          (Int32.shift_left (Int32.of_int b3) 24)))

let rec execute vm =
  let proc = vm.procs.(vm.current_proc) in
  let code_list = proc.code in
  let code_bytes =
    List.fold_left
      (fun acc instr -> Bytes.cat acc (Bytecode.encode_instr instr))
      Bytes.empty
      code_list
  in
  let code = Bytes.to_string code_bytes in
  let code_len = String.length code in

  while vm.ip < code_len do
    let opcode = Char.code code.[vm.ip] in
    vm.ip <- vm.ip + 1;

    match opcode with
    | 0x00 -> ()
    | 0x02 -> push vm Value.Unit
    | 0x04 -> push vm (Value.Int (-1))
    | 0x05 -> push vm (Value.Int 0)
    | 0x06 -> push vm (Value.Int 1)
    | 0x07 -> push vm (Value.Int 2)
    | 0x08 -> push vm (Value.Int 3)
    | 0x09 -> push vm (Value.Int 4)
    | 0x0A -> push vm (Value.Int 5)
    | 0x0B -> push vm (Value.Int 6)
    | 0x0C -> push vm (Value.Int 7)
    | 0x0D -> push vm (Value.Int 8)
    | 0x0E ->
      let n = Char.code code.[vm.ip] in
      vm.ip <- vm.ip + 1;
      push vm (Value.Int n)
    | 0x0F ->
      let n = decode_i4 code vm.ip in
      vm.ip <- vm.ip + 4;
      push vm (Value.Int n)
    | 0x13 -> (
      let idx = decode_i4 code vm.ip in
      vm.ip <- vm.ip + 4;
      match vm.constants.(idx) with
      | CText s ->
        check_gc vm;
        push vm (Memory.make_text s)
      | _ -> raise (Runtime_error "expected 'Text' constant"))
    | 0x14 ->
      let value = peek vm in
      push vm value
    | 0x15 ->
      let value = pop vm in
      Memory.release value
    | 0x20 -> (
      match vm.frames with
      | frame :: _ -> push vm frame.locals.(0)
      | [] -> raise (Runtime_error "no frame"))
    | 0x21 -> (
      match vm.frames with
      | frame :: _ -> push vm frame.locals.(1)
      | [] -> raise (Runtime_error "no frame"))
    | 0x22 -> (
      match vm.frames with
      | frame :: _ -> push vm frame.locals.(2)
      | [] -> raise (Runtime_error "no frame"))
    | 0x23 -> (
      match vm.frames with
      | frame :: _ -> push vm frame.locals.(3)
      | [] -> raise (Runtime_error "no frame"))
    | 0x24 -> (
      let idx = Char.code code.[vm.ip] in
      vm.ip <- vm.ip + 1;
      match vm.frames with
      | frame :: _ -> push vm frame.locals.(idx)
      | [] -> raise (Runtime_error "no frame"))
    | 0x28 -> (
      let value = pop vm in
      match vm.frames with
      | frame :: _ ->
        Memory.release frame.locals.(0);
        frame.locals.(0) <- value
      | [] -> raise (Runtime_error "no frame"))
    | 0x29 -> (
      let value = pop vm in
      match vm.frames with
      | frame :: _ ->
        Memory.release frame.locals.(1);
        frame.locals.(1) <- value
      | [] -> raise (Runtime_error "no frame"))
    | 0x2A -> (
      let value = pop vm in
      match vm.frames with
      | frame :: _ ->
        Memory.release frame.locals.(2);
        frame.locals.(2) <- value
      | [] -> raise (Runtime_error "no frame"))
    | 0x2B -> (
      let value = pop vm in
      match vm.frames with
      | frame :: _ ->
        Memory.release frame.locals.(3);
        frame.locals.(3) <- value
      | [] -> raise (Runtime_error "no frame"))
    | 0x2C -> (
      let idx = Char.code code.[vm.ip] in
      vm.ip <- vm.ip + 1;
      let value = pop vm in
      match vm.frames with
      | frame :: _ ->
        Memory.release frame.locals.(idx);
        frame.locals.(idx) <- value
      | [] -> raise (Runtime_error "no frame"))
    | 0x43 -> (
      let offset = Int32.to_int (Int32.of_int (decode_i4 code vm.ip)) in
      vm.ip <- vm.ip + 4;
      let cond = pop vm in
      match cond with
      | Value.Bool false -> vm.ip <- vm.ip + offset
      | Value.Bool true -> Memory.release cond
      | _ -> raise (Runtime_error "'brfalse' expects 'Bool'"))
    | 0x45 -> (
      let offset = Int32.to_int (Int32.of_int (decode_i4 code vm.ip)) in
      vm.ip <- vm.ip + 4;
      let cond = pop vm in
      match cond with
      | Value.Bool true -> vm.ip <- vm.ip + offset
      | Value.Bool false -> Memory.release cond
      | _ -> raise (Runtime_error "'brtrue' expects 'Bool'"))
    | 0x60 -> (
      let b = pop vm in
      let a = pop vm in
      match (a, b) with
      | Value.Int x, Value.Int y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Int (x + y))
      | Value.Nat x, Value.Nat y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Nat (x + y))
      | _ -> raise (Runtime_error "'add' type mismatch"))
    | 0x62 -> (
      let b = pop vm in
      let a = pop vm in
      match (a, b) with
      | Value.Int x, Value.Int y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Int (x - y))
      | Value.Nat x, Value.Nat y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Nat (x - y))
      | _ -> raise (Runtime_error "'sub' type mismatch"))
    | 0x64 -> (
      let b = pop vm in
      let a = pop vm in
      match (a, b) with
      | Value.Int x, Value.Int y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Int (x * y))
      | Value.Nat x, Value.Nat y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Nat (x * y))
      | _ -> raise (Runtime_error "'mul' type mismatch"))
    | 0x66 -> (
      let b = pop vm in
      let a = pop vm in
      match (a, b) with
      | Value.Int x, Value.Int y ->
        if y = 0 then raise (Runtime_error "division by zero");
        Memory.release a;
        Memory.release b;
        push vm (Value.Int (x / y))
      | Value.Nat x, Value.Nat y ->
        if y = 0 then raise (Runtime_error "division by zero");
        Memory.release a;
        Memory.release b;
        push vm (Value.Nat (x / y))
      | _ -> raise (Runtime_error "'div' type mismatch"))
    | 0x67 -> (
      let b = pop vm in
      let a = pop vm in
      let rem_euclid x y =
        let r = x mod y in
        if r < 0 then if y > 0 then r + y else r - y else r
      in
      match (a, b) with
      | Value.Int x, Value.Int y ->
        if y = 0 then raise (Runtime_error "Euclidean division by zero");
        Memory.release a;
        Memory.release b;
        push vm (Value.Int (rem_euclid x y))
      | Value.Nat x, Value.Nat y ->
        if y = 0 then raise (Runtime_error "Euclidean division by zero");
        Memory.release a;
        Memory.release b;
        push vm (Value.Nat (rem_euclid x y))
      | _ -> raise (Runtime_error "'mod' type mismatch"))
    | 0x69 -> (
      let a = pop vm in
      match a with
      | Value.Int x ->
        Memory.release a;
        push vm (Value.Int (-x))
      | _ -> raise (Runtime_error "'neg' expects 'Int'"))
    | 0x80 ->
      let b = pop vm in
      let a = pop vm in
      let result =
        match (a, b) with
        | Value.Unit, Value.Unit -> true
        | Value.Bool x, Value.Bool y -> x = y
        | Value.Int x, Value.Int y -> x = y
        | Value.Nat x, Value.Nat y -> x = y
        | _ -> false
      in
      Memory.release a;
      Memory.release b;
      push vm (Value.Bool result)
    | 0x81 -> (
      let b = pop vm in
      let a = pop vm in
      match (a, b) with
      | Value.Int x, Value.Int y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Bool (x > y))
      | Value.Nat x, Value.Nat y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Bool (x > y))
      | _ -> raise (Runtime_error "'cgt' type mismatch"))
    | 0x82 -> (
      let b = pop vm in
      let a = pop vm in
      match (a, b) with
      | Value.Int x, Value.Int y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Bool (x < y))
      | Value.Nat x, Value.Nat y ->
        Memory.release a;
        Memory.release b;
        push vm (Value.Bool (x < y))
      | _ -> raise (Runtime_error "'clt' type mismatch"))
    | 0x73 -> (
      let a = pop vm in
      match a with
      | Value.Bool b ->
        Memory.release a;
        push vm (Value.Bool (not b))
      | _ -> raise (Runtime_error "'not' expects 'Bool'"))
    | 0xF0 ->
      let proc_id = decode_i4 code vm.ip in
      vm.ip <- vm.ip + 4;
      let target_proc = vm.procs.(proc_id) in
      if target_proc.external_proc then
        call_extern vm target_proc.name target_proc.param_count
      else
        let args = Array.init target_proc.param_count (fun _ -> pop vm) in
        Array.iter Memory.retain args;
        let locals =
          Array.init target_proc.local_count (fun i ->
            if i < target_proc.param_count then
              args.(target_proc.param_count - 1 - i)
            else Value.Unit)
        in
        let frame =
          {
            return_ip = vm.ip
          ; return_proc = vm.current_proc
          ; base_pointer = vm.sp
          ; locals
          }
        in
        vm.frames <- frame :: vm.frames;
        vm.current_proc <- proc_id;
        vm.ip <- 0;
        execute vm
    | 0xF3 -> (
      let return_value = if vm.sp > 0 then pop vm else Value.Unit in
      match vm.frames with
      | [ _ ] ->
        Memory.release return_value;
        vm.ip <- code_len
      | frame :: rest ->
        Array.iter Memory.release frame.locals;
        vm.frames <- rest;
        vm.ip <- frame.return_ip;
        vm.current_proc <- frame.return_proc;
        push vm return_value
      | [] ->
        Memory.release return_value;
        vm.ip <- code_len)
    | _ ->
      raise (Runtime_error (Printf.sprintf "unknown opcode '0x%02X'" opcode))
  done

let run vm =
  try
    let entry_proc = vm.procs.(0) in
    let locals = Array.make entry_proc.local_count Value.Unit in
    let frame = { return_ip = 0; return_proc = 0; base_pointer = 0; locals } in
    vm.frames <- [ frame ];
    execute vm;
    0
  with Runtime_error msg ->
    Printf.eprintf "Runtime Error: %s\n" msg;
    1
