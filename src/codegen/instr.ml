type instr =
  (* Stack constants *)
  | Nop
  | LdcI4M1
  | LdcI4_0
  | LdcI4_1
  | LdcI4_2
  | LdcI4_3
  | LdcI4_4
  | LdcI4_5
  | LdcI4_6
  | LdcI4_7
  | LdcI4_8
  | LdcI4S of int
  | LdcI4 of int32
  | LdcI8 of int64
  | LdcR4 of float
  | LdcR8 of float
  | Ldstr of int
  | LdUnit
  | Dup
  | Pop
  (* Locals *)
  | Ldloc_0
  | Ldloc_1
  | Ldloc_2
  | Ldloc_3
  | LdlocS of int
  | Ldloc of int
  | Stloc_0
  | Stloc_1
  | Stloc_2
  | Stloc_3
  | StlocS of int
  | Stloc of int
  (* Arguments *)
  | Ldarg_0
  | Ldarg_1
  | Ldarg_2
  | Ldarg_3
  | LdargS of int
  | Ldarg of int
  | Starg_0
  | Starg_1
  | Starg_2
  | Starg_3
  | StargS of int
  | Starg of int
  (* Branching *)
  | BrS of int
  | Br of int32
  | BrfalseS of int
  | Brfalse of int32
  | BrtrueS of int
  | Brtrue of int32
  | Beq of int32
  | Bne of int32
  | Blt of int32
  | Ble of int32
  | Bgt of int32
  | Bge of int32
  | Switch of int32 list
  (* Arithmetic *)
  | Add
  | AddOvf
  | Sub
  | SubOvf
  | Mul
  | MulOvf
  | Div
  | Mod
  | Pow
  | Neg
  (* Comparison *)
  | Ceq
  | Cgt
  | Clt
  (* Bitwise *)
  | And
  | Or
  | Xor
  | Not
  | Shl
  | Shr
  (* Conversions *)
  | ConvI1
  | ConvI2
  | ConvI4
  | ConvI8
  | ConvN1
  | ConvN2
  | ConvN4
  | ConvN8
  | ConvR4
  | ConvR8
  (* Object operations *)
  | Newobj of int
  | Newarr of int
  | Ldfld of int
  | Stfld of int
  | Ldelem of int
  | Stelem of int
  | Ldlen
  | Box of int
  | Unbox of int
  (* Pointer operations: indirect load *)
  | LdindI1
  | LdindI2
  | LdindI4
  | LdindI8
  | LdindN1
  | LdindN2
  | LdindN4
  | LdindN8
  | LdindR4
  | LdindR8
  | LdindRef
  (* Pointer operations: indirect store *)
  | StindI1
  | StindI2
  | StindI4
  | StindI8
  | StindR4
  | StindR8
  | StindRef
  (* Pointer operations: address operations *)
  | Ldloca of int
  | Ldarga of int
  | Ldflda of int
  | Ldelema of int
  | Sizeof of int
  | Localloc
  (* Type introspection *)
  | Isinst of int
  | Castclass of int
  | Ldtoken of int
  (* Null handling *)
  | Ldnull
  | Brnull of int32
  | Brnonnull of int32
  (* Control plane *)
  | Call of int
  | Calli of int
  | Callvirt of int
  | Ret
  | Jmp of int
  (* Structured exceptions *)
  | Throw
  | Rethrow
  | Leave of int32
  (* Extension opcodes (0xFE) *)
  | Defer of int
  | GcRetain
  | GcRelease
  | GcAutorelease

type constant = CInt32 of int32 | CText of string

type proc_def = {
    name : string
  ; param_count : int
  ; local_count : int
  ; code : instr list
}

type record_type_def = { name : string; fields : (string * string) list }

type program = {
    constants : constant list
  ; procs : proc_def list
  ; records : record_type_def list
}

let encode_instr = function
  | Nop -> Bytes.of_string "\x00"
  | LdcI4M1 -> Bytes.of_string "\x04"
  | LdcI4_0 -> Bytes.of_string "\x05"
  | LdcI4_1 -> Bytes.of_string "\x06"
  | LdcI4_2 -> Bytes.of_string "\x07"
  | LdcI4_3 -> Bytes.of_string "\x08"
  | LdcI4_4 -> Bytes.of_string "\x09"
  | LdcI4_5 -> Bytes.of_string "\x0A"
  | LdcI4_6 -> Bytes.of_string "\x0B"
  | LdcI4_7 -> Bytes.of_string "\x0C"
  | LdcI4_8 -> Bytes.of_string "\x0D"
  | LdcI4S n ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\x0E';
    Bytes.set b 1 (Char.chr n);
    b
  | LdcI4 n ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x0F';
    Bytes.set_int32_le b 1 n;
    b
  | LdcI8 n ->
    let b = Bytes.create 9 in
    Bytes.set b 0 '\x10';
    Bytes.set_int64_le b 1 n;
    b
  | LdcR4 f ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x11';
    Bytes.set_int32_le b 1 (Int32.bits_of_float f);
    b
  | LdcR8 f ->
    let b = Bytes.create 9 in
    Bytes.set b 0 '\x12';
    Bytes.set_int64_le b 1 (Int64.bits_of_float f);
    b
  | Ldstr idx ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x13';
    Bytes.set_int32_le b 1 (Int32.of_int idx);
    b
  | LdUnit -> Bytes.of_string "\x02"
  | Dup -> Bytes.of_string "\x14"
  | Pop -> Bytes.of_string "\x15"
  | Ldloc_0 -> Bytes.of_string "\x20"
  | Ldloc_1 -> Bytes.of_string "\x21"
  | Ldloc_2 -> Bytes.of_string "\x22"
  | Ldloc_3 -> Bytes.of_string "\x23"
  | LdlocS n ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\x24';
    Bytes.set b 1 (Char.chr n);
    b
  | Ldloc n ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x25';
    Bytes.set_int32_le b 1 (Int32.of_int n);
    b
  | Stloc_0 -> Bytes.of_string "\x28"
  | Stloc_1 -> Bytes.of_string "\x29"
  | Stloc_2 -> Bytes.of_string "\x2A"
  | Stloc_3 -> Bytes.of_string "\x2B"
  | StlocS n ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\x2C';
    Bytes.set b 1 (Char.chr n);
    b
  | Stloc n ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x2D';
    Bytes.set_int32_le b 1 (Int32.of_int n);
    b
  | Ldarg_0 -> Bytes.of_string "\x30"
  | Ldarg_1 -> Bytes.of_string "\x31"
  | Ldarg_2 -> Bytes.of_string "\x32"
  | Ldarg_3 -> Bytes.of_string "\x33"
  | LdargS n ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\x34';
    Bytes.set b 1 (Char.chr n);
    b
  | Ldarg n ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x35';
    Bytes.set_int32_le b 1 (Int32.of_int n);
    b
  | Starg_0 -> Bytes.of_string "\x36"
  | Starg_1 -> Bytes.of_string "\x37"
  | Starg_2 -> Bytes.of_string "\x38"
  | Starg_3 -> Bytes.of_string "\x39"
  | StargS n ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\x3A';
    Bytes.set b 1 (Char.chr n);
    b
  | Starg n ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x3B';
    Bytes.set_int32_le b 1 (Int32.of_int n);
    b
  | BrS offset ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\x40';
    Bytes.set b 1 (Char.chr offset);
    b
  | Br offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x41';
    Bytes.set_int32_le b 1 offset;
    b
  | BrfalseS offset ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\x42';
    Bytes.set b 1 (Char.chr offset);
    b
  | Brfalse offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x43';
    Bytes.set_int32_le b 1 offset;
    b
  | BrtrueS offset ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\x44';
    Bytes.set b 1 (Char.chr offset);
    b
  | Brtrue offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x45';
    Bytes.set_int32_le b 1 offset;
    b
  | Beq offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x46';
    Bytes.set_int32_le b 1 offset;
    b
  | Bne offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x47';
    Bytes.set_int32_le b 1 offset;
    b
  | Blt offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x48';
    Bytes.set_int32_le b 1 offset;
    b
  | Ble offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x49';
    Bytes.set_int32_le b 1 offset;
    b
  | Bgt offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x4A';
    Bytes.set_int32_le b 1 offset;
    b
  | Bge offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\x4B';
    Bytes.set_int32_le b 1 offset;
    b
  | Switch targets ->
    let count = List.length targets in
    let b = Bytes.create (5 + (count * 4)) in
    Bytes.set b 0 '\x4C';
    Bytes.set_int32_le b 1 (Int32.of_int count);
    List.iteri
      (fun i offset -> Bytes.set_int32_le b (5 + (i * 4)) offset)
      targets;
    b
  | Add -> Bytes.of_string "\x60"
  | AddOvf -> Bytes.of_string "\x61"
  | Sub -> Bytes.of_string "\x62"
  | SubOvf -> Bytes.of_string "\x63"
  | Mul -> Bytes.of_string "\x64"
  | MulOvf -> Bytes.of_string "\x65"
  | Div -> Bytes.of_string "\x66"
  | Mod -> Bytes.of_string "\x67"
  | Pow -> Bytes.of_string "\x68"
  | Neg -> Bytes.of_string "\x69"
  | Ceq -> Bytes.of_string "\x80"
  | Cgt -> Bytes.of_string "\x81"
  | Clt -> Bytes.of_string "\x82"
  | And -> Bytes.of_string "\x70"
  | Or -> Bytes.of_string "\x71"
  | Xor -> Bytes.of_string "\x72"
  | Not -> Bytes.of_string "\x73"
  | Shl -> Bytes.of_string "\x74"
  | Shr -> Bytes.of_string "\x75"
  | ConvI1 -> Bytes.of_string "\x90"
  | ConvI2 -> Bytes.of_string "\x91"
  | ConvI4 -> Bytes.of_string "\x92"
  | ConvI8 -> Bytes.of_string "\x93"
  | ConvN1 -> Bytes.of_string "\x94"
  | ConvN2 -> Bytes.of_string "\x95"
  | ConvN4 -> Bytes.of_string "\x96"
  | ConvN8 -> Bytes.of_string "\x97"
  | ConvR4 -> Bytes.of_string "\x98"
  | ConvR8 -> Bytes.of_string "\x99"
  | Newobj token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xA0';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Newarr token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xA1';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Ldfld token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xA2';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Stfld token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xA3';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Ldelem token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xA4';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Stelem token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xA5';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Ldlen -> Bytes.of_string "\xA6"
  | Box token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xA7';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Unbox token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xA8';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | LdindI1 -> Bytes.of_string "\xB0"
  | LdindI2 -> Bytes.of_string "\xB1"
  | LdindI4 -> Bytes.of_string "\xB2"
  | LdindI8 -> Bytes.of_string "\xB3"
  | LdindN1 -> Bytes.of_string "\xB4"
  | LdindN2 -> Bytes.of_string "\xB5"
  | LdindN4 -> Bytes.of_string "\xB6"
  | LdindN8 -> Bytes.of_string "\xB7"
  | LdindR4 -> Bytes.of_string "\xB8"
  | LdindR8 -> Bytes.of_string "\xB9"
  | LdindRef -> Bytes.of_string "\xBA"
  | StindI1 -> Bytes.of_string "\xC0"
  | StindI2 -> Bytes.of_string "\xC1"
  | StindI4 -> Bytes.of_string "\xC2"
  | StindI8 -> Bytes.of_string "\xC3"
  | StindR4 -> Bytes.of_string "\xC4"
  | StindR8 -> Bytes.of_string "\xC5"
  | StindRef -> Bytes.of_string "\xC6"
  | Ldloca n ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xD0';
    Bytes.set_int32_le b 1 (Int32.of_int n);
    b
  | Ldarga n ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xD1';
    Bytes.set_int32_le b 1 (Int32.of_int n);
    b
  | Ldflda token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xD2';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Ldelema token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xD3';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Sizeof token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xD4';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Localloc -> Bytes.of_string "\xD5"
  | Isinst token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xE0';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Castclass token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xE1';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Ldtoken token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xE2';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Ldnull -> Bytes.of_string "\xE3"
  | Brnull offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xE4';
    Bytes.set_int32_le b 1 offset;
    b
  | Brnonnull offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xE5';
    Bytes.set_int32_le b 1 offset;
    b
  | Call proc_id ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xF0';
    Bytes.set_int32_le b 1 (Int32.of_int proc_id);
    b
  | Calli token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xF1';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Callvirt token ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xF2';
    Bytes.set_int32_le b 1 (Int32.of_int token);
    b
  | Ret -> Bytes.of_string "\xF3"
  | Jmp proc_id ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xF4';
    Bytes.set_int32_le b 1 (Int32.of_int proc_id);
    b
  | Throw -> Bytes.of_string "\xF5"
  | Rethrow -> Bytes.of_string "\xF6"
  | Leave offset ->
    let b = Bytes.create 5 in
    Bytes.set b 0 '\xF7';
    Bytes.set_int32_le b 1 offset;
    b
  | Defer proc_id ->
    let b = Bytes.create 6 in
    Bytes.set b 0 '\xFE';
    Bytes.set b 1 '\x00';
    Bytes.set_int32_le b 2 (Int32.of_int proc_id);
    b
  | GcRetain ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\xFE';
    Bytes.set b 1 '\x01';
    b
  | GcRelease ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\xFE';
    Bytes.set b 1 '\x02';
    b
  | GcAutorelease ->
    let b = Bytes.create 2 in
    Bytes.set b 0 '\xFE';
    Bytes.set b 1 '\x03';
    b

let serialize_proc (proc : proc_def) =
  let name_bytes = Bytes.of_string proc.name in
  let name_len = Bytes.length name_bytes in

  let code_bytes =
    List.fold_left
      (fun acc instr -> Bytes.cat acc (encode_instr instr))
      Bytes.empty
      proc.code
  in

  let result = Bytes.create (4 + name_len + 16 + Bytes.length code_bytes) in
  let pos = ref 0 in

  (* name len *)
  Bytes.set_int32_le result !pos (Int32.of_int name_len);
  pos := !pos + 4;

  (* name *)
  Bytes.blit_string proc.name 0 result !pos name_len;
  pos := !pos + name_len;

  (* param count *)
  Bytes.set_int32_le result !pos (Int32.of_int proc.param_count);
  pos := !pos + 4;

  (* local count *)
  Bytes.set_int32_le result !pos (Int32.of_int proc.local_count);
  pos := !pos + 4;

  Bytes.set_int32_le result !pos (Int32.of_int (proc.local_count * 2));
  pos := !pos + 4;

  (* code size *)
  Bytes.set_int32_le result !pos (Int32.of_int (Bytes.length code_bytes));
  pos := !pos + 4;

  (* code *)
  Bytes.blit code_bytes 0 result !pos (Bytes.length code_bytes);

  result

let serialize_proc procs =
  let count_bytes = Bytes.create 4 in
  Bytes.set_int32_le count_bytes 0 (Int32.of_int (List.length procs));

  let proc_bytes =
    List.fold_left
      (fun acc proc -> Bytes.cat acc (serialize_proc proc))
      Bytes.empty
      procs
  in

  Bytes.cat count_bytes proc_bytes

let serialize_record_type record =
  let name_bytes = Bytes.of_string record.name in
  let name_len = Bytes.length name_bytes in
  let field_count = List.length record.fields in

  let fields_bytes =
    List.fold_left
      (fun acc (field_name, field_type) ->
        let field_name_bytes = Bytes.of_string field_name in
        let field_type_bytes = Bytes.of_string field_type in
        let field_name_len = Bytes.length field_name_bytes in
        let field_type_len = Bytes.length field_type_bytes in
        let field_bytes = Bytes.create (8 + field_name_len + field_type_len) in
        Bytes.set_int32_le field_bytes 0 (Int32.of_int field_name_len);
        Bytes.blit field_name_bytes 0 field_bytes 4 field_name_len;
        Bytes.set_int32_le
          field_bytes
          (4 + field_name_len)
          (Int32.of_int field_type_len);
        Bytes.blit
          field_type_bytes
          0
          field_bytes
          (8 + field_name_len)
          field_type_len;
        Bytes.cat acc field_bytes)
      Bytes.empty
      record.fields
  in

  let result = Bytes.create (8 + name_len + Bytes.length fields_bytes) in
  Bytes.set_int32_le result 0 (Int32.of_int name_len);
  Bytes.blit name_bytes 0 result 4 name_len;
  Bytes.set_int32_le result (4 + name_len) (Int32.of_int field_count);
  Bytes.blit fields_bytes 0 result (8 + name_len) (Bytes.length fields_bytes);
  result

let serialize_record_types records =
  let count_bytes = Bytes.create 4 in
  Bytes.set_int32_le count_bytes 0 (Int32.of_int (List.length records));

  let record_bytes =
    List.fold_left
      (fun acc record -> Bytes.cat acc (serialize_record_type record))
      Bytes.empty
      records
  in

  Bytes.cat count_bytes record_bytes

let encode_program program =
  let header = Bytes.create 32 in
  Bytes.blit_string "MUSI" 0 header 0 4;
  Bytes.set_int16_le header 4 1;
  Bytes.set_int16_le header 6 0;

  let serialize_constant constants =
    let count = List.length constants in
    let count_bytes = Bytes.create 4 in
    Bytes.set_int32_le count_bytes 0 (Int32.of_int count);

    let total_const_size =
      List.fold_left
        (fun acc const ->
          let const_bytes =
            match const with
            | CInt32 value ->
              let result = Bytes.create 5 in
              Bytes.set_uint8 result 0 0x01;
              Bytes.set_int32_le result 1 value;
              result
            | CText text ->
              let text_len = String.length text in
              let result = Bytes.create (5 + text_len) in
              Bytes.set_uint8 result 0 0x05;
              Bytes.set_int32_le result 1 (Int32.of_int text_len);
              Bytes.blit_string text 0 result 5 text_len;
              result
          in
          acc + Bytes.length const_bytes)
        0
        constants
    in

    let result = Bytes.create (4 + total_const_size) in
    Bytes.blit count_bytes 0 result 0 4;

    let _ =
      List.fold_left
        (fun offset const ->
          let const_bytes =
            match const with
            | CInt32 value ->
              let result = Bytes.create 5 in
              Bytes.set_uint8 result 0 0x01;
              Bytes.set_int32_le result 1 value;
              result
            | CText text ->
              let text_len = String.length text in
              let result = Bytes.create (5 + text_len) in
              Bytes.set_uint8 result 0 0x05;
              Bytes.set_int32_le result 1 (Int32.of_int text_len);
              Bytes.blit_string text 0 result 5 text_len;
              result
          in
          let const_len = Bytes.length const_bytes in
          Bytes.blit const_bytes 0 result offset const_len;
          offset + const_len)
        4
        constants
    in

    result
  in
  let constant_bytes = serialize_constant program.constants in
  let type_bytes = serialize_record_types program.records in
  let proc_bytes = serialize_proc program.procs in
  let constant_offset = 32 in
  let type_metadata_offset = constant_offset + Bytes.length constant_bytes in
  let proc_offset = type_metadata_offset + Bytes.length type_bytes in
  let entry_point = 0 in
  let debug_offset = proc_offset + Bytes.length proc_bytes in

  Bytes.set_int32_le header 8 0l;
  Bytes.set_int32_le header 12 (Int32.of_int constant_offset);
  Bytes.set_int32_le header 16 (Int32.of_int type_metadata_offset);
  Bytes.set_int32_le header 20 (Int32.of_int proc_offset);
  Bytes.set_int32_le header 24 (Int32.of_int entry_point);
  Bytes.set_int32_le header 28 (Int32.of_int debug_offset);

  Bytes.cat header (Bytes.cat constant_bytes (Bytes.cat type_bytes proc_bytes))
