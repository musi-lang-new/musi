open Instr

let encode_with_payload size opcode setter =
  let b = Bytes.create size in
  Bytes.set b 0 opcode;
  setter b;
  b

let encode_switch_instr targets =
  let count = List.length targets in
  let b = Bytes.create (5 + (count * 4)) in
  Bytes.set b 0 '\x4C';
  Bytes.set_int32_le b 1 (Int32.of_int count);
  List.iteri (fun i offset -> Bytes.set_int32_le b (5 + (i * 4)) offset) targets;
  b

let encode_ldtoken_instr opcode token =
  encode_with_payload 5 opcode (fun b ->
    Bytes.set_int32_le b 1 (Int32.of_int token))

let encode_defer_instr proc_id =
  let b = Bytes.create 6 in
  Bytes.set b 0 '\xFE';
  Bytes.set b 1 '\x00';
  Bytes.set_int32_le b 2 (Int32.of_int proc_id);
  b

let encode_gc_instr subcode =
  let b = Bytes.create 2 in
  Bytes.set b 0 '\xFE';
  Bytes.set b 1 subcode;
  b

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
    encode_with_payload 2 '\x0E' (fun b -> Bytes.set b 1 (Char.chr n))
  | LdcI4 n -> encode_with_payload 5 '\x0F' (fun b -> Bytes.set_int32_le b 1 n)
  | LdcI8 n -> encode_with_payload 9 '\x10' (fun b -> Bytes.set_int64_le b 1 n)
  | LdcR4 f ->
    encode_with_payload 5 '\x11' (fun b ->
      Bytes.set_int32_le b 1 (Int32.bits_of_float f))
  | LdcR8 f ->
    encode_with_payload 9 '\x12' (fun b ->
      Bytes.set_int64_le b 1 (Int64.bits_of_float f))
  | Ldstr idx ->
    encode_with_payload 5 '\x13' (fun b ->
      Bytes.set_int32_le b 1 (Int32.of_int idx))
  | LdUnit -> Bytes.of_string "\x02"
  | Dup -> Bytes.of_string "\x14"
  | Pop -> Bytes.of_string "\x15"
  | Ldloc_0 -> Bytes.of_string "\x20"
  | Ldloc_1 -> Bytes.of_string "\x21"
  | Ldloc_2 -> Bytes.of_string "\x22"
  | Ldloc_3 -> Bytes.of_string "\x23"
  | LdlocS n ->
    encode_with_payload 2 '\x24' (fun b -> Bytes.set b 1 (Char.chr n))
  | Ldloc n ->
    encode_with_payload 5 '\x25' (fun b ->
      Bytes.set_int32_le b 1 (Int32.of_int n))
  | Stloc_0 -> Bytes.of_string "\x28"
  | Stloc_1 -> Bytes.of_string "\x29"
  | Stloc_2 -> Bytes.of_string "\x2A"
  | Stloc_3 -> Bytes.of_string "\x2B"
  | StlocS n ->
    encode_with_payload 2 '\x2C' (fun b -> Bytes.set b 1 (Char.chr n))
  | Stloc n ->
    encode_with_payload 5 '\x2D' (fun b ->
      Bytes.set_int32_le b 1 (Int32.of_int n))
  | Ldarg_0 -> Bytes.of_string "\x30"
  | Ldarg_1 -> Bytes.of_string "\x31"
  | Ldarg_2 -> Bytes.of_string "\x32"
  | Ldarg_3 -> Bytes.of_string "\x33"
  | LdargS n ->
    encode_with_payload 2 '\x34' (fun b -> Bytes.set b 1 (Char.chr n))
  | Ldarg n ->
    encode_with_payload 5 '\x35' (fun b ->
      Bytes.set_int32_le b 1 (Int32.of_int n))
  | Starg_0 -> Bytes.of_string "\x36"
  | Starg_1 -> Bytes.of_string "\x37"
  | Starg_2 -> Bytes.of_string "\x38"
  | Starg_3 -> Bytes.of_string "\x39"
  | StargS n ->
    encode_with_payload 2 '\x3A' (fun b -> Bytes.set b 1 (Char.chr n))
  | Starg n ->
    encode_with_payload 5 '\x3B' (fun b ->
      Bytes.set_int32_le b 1 (Int32.of_int n))
  | BrS offset ->
    encode_with_payload 2 '\x40' (fun b -> Bytes.set b 1 (Char.chr offset))
  | Br offset ->
    encode_with_payload 5 '\x41' (fun b -> Bytes.set_int32_le b 1 offset)
  | BrfalseS offset ->
    encode_with_payload 2 '\x42' (fun b -> Bytes.set b 1 (Char.chr offset))
  | Brfalse offset ->
    encode_with_payload 5 '\x43' (fun b -> Bytes.set_int32_le b 1 offset)
  | BrtrueS offset ->
    encode_with_payload 2 '\x44' (fun b -> Bytes.set b 1 (Char.chr offset))
  | Brtrue offset ->
    encode_with_payload 5 '\x45' (fun b -> Bytes.set_int32_le b 1 offset)
  | Beq offset ->
    encode_with_payload 5 '\x46' (fun b -> Bytes.set_int32_le b 1 offset)
  | Bne offset ->
    encode_with_payload 5 '\x47' (fun b -> Bytes.set_int32_le b 1 offset)
  | Blt offset ->
    encode_with_payload 5 '\x48' (fun b -> Bytes.set_int32_le b 1 offset)
  | Ble offset ->
    encode_with_payload 5 '\x49' (fun b -> Bytes.set_int32_le b 1 offset)
  | Bgt offset ->
    encode_with_payload 5 '\x4A' (fun b -> Bytes.set_int32_le b 1 offset)
  | Bge offset ->
    encode_with_payload 5 '\x4B' (fun b -> Bytes.set_int32_le b 1 offset)
  | Switch targets -> encode_switch_instr targets
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
  | Newobj token -> encode_ldtoken_instr '\xA0' token
  | Newarr token -> encode_ldtoken_instr '\xA1' token
  | Ldfld token -> encode_ldtoken_instr '\xA2' token
  | Stfld token -> encode_ldtoken_instr '\xA3' token
  | Ldelem token -> encode_ldtoken_instr '\xA4' token
  | Stelem token -> encode_ldtoken_instr '\xA5' token
  | Ldlen -> Bytes.of_string "\xA6"
  | Box token -> encode_ldtoken_instr '\xA7' token
  | Unbox token -> encode_ldtoken_instr '\xA8' token
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
  | Ldloca n -> encode_ldtoken_instr '\xD0' n
  | Ldarga n -> encode_ldtoken_instr '\xD1' n
  | Ldflda token -> encode_ldtoken_instr '\xD2' token
  | Ldelema token -> encode_ldtoken_instr '\xD3' token
  | Sizeof token -> encode_ldtoken_instr '\xD4' token
  | Localloc -> Bytes.of_string "\xD5"
  | Isinst token -> encode_ldtoken_instr '\xE0' token
  | Castclass token -> encode_ldtoken_instr '\xE1' token
  | Ldtoken token -> encode_ldtoken_instr '\xE2' token
  | Ldnull -> Bytes.of_string "\xE3"
  | Brnull offset ->
    encode_with_payload 5 '\xE4' (fun b -> Bytes.set_int32_le b 1 offset)
  | Brnonnull offset ->
    encode_with_payload 5 '\xE5' (fun b -> Bytes.set_int32_le b 1 offset)
  | Call proc_id -> encode_ldtoken_instr '\xF0' proc_id
  | Calli token -> encode_ldtoken_instr '\xF1' token
  | Callvirt token -> encode_ldtoken_instr '\xF2' token
  | Ret -> Bytes.of_string "\xF3"
  | Jmp proc_id -> encode_ldtoken_instr '\xF4' proc_id
  | Throw -> Bytes.of_string "\xF5"
  | Rethrow -> Bytes.of_string "\xF6"
  | Leave offset ->
    encode_with_payload 5 '\xF7' (fun b -> Bytes.set_int32_le b 1 offset)
  | Defer proc_id -> encode_defer_instr proc_id
  | GcRetain -> encode_gc_instr '\x01'
  | GcRelease -> encode_gc_instr '\x02'
  | GcAutorelease -> encode_gc_instr '\x03'

let encode_constant = function
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

let encode_proc (proc : proc_def) =
  let name_bytes = Bytes.of_string proc.name in
  let name_len = Bytes.length name_bytes in
  let code_bytes =
    List.fold_left
      (fun acc instr -> Bytes.cat acc (encode_instr instr))
      Bytes.empty
      proc.code
  in
  let result = Bytes.create (4 + name_len + 20 + Bytes.length code_bytes) in
  let pos = ref 0 in
  Bytes.set_int32_le result !pos (Int32.of_int name_len);
  pos := !pos + 4;
  Bytes.blit_string proc.name 0 result !pos name_len;
  pos := !pos + name_len;
  Bytes.set_int32_le result !pos (Int32.of_int proc.param_count);
  pos := !pos + 4;
  Bytes.set_int32_le result !pos (Int32.of_int proc.local_count);
  pos := !pos + 4;
  Bytes.set_int32_le result !pos (Int32.of_int (proc.local_count * 2));
  pos := !pos + 4;
  Bytes.set_int32_le result !pos (if proc.external_proc then 1l else 0l);
  pos := !pos + 4;
  Bytes.set_int32_le result !pos (Int32.of_int (Bytes.length code_bytes));
  pos := !pos + 4;
  Bytes.blit code_bytes 0 result !pos (Bytes.length code_bytes);
  result

let encode_procs procs =
  let count_bytes = Bytes.create 4 in
  Bytes.set_int32_le count_bytes 0 (Int32.of_int (Array.length procs));
  let proc_bytes =
    Array.fold_left
      (fun acc proc -> Bytes.cat acc (encode_proc proc))
      Bytes.empty
      procs
  in
  Bytes.cat count_bytes proc_bytes

let encode_record_type record =
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

let encode_record_types records =
  let count_bytes = Bytes.create 4 in
  Bytes.set_int32_le count_bytes 0 (Int32.of_int (List.length records));
  let record_bytes =
    List.fold_left
      (fun acc record -> Bytes.cat acc (encode_record_type record))
      Bytes.empty
      records
  in
  Bytes.cat count_bytes record_bytes

let encode_constants constants =
  let count = Array.length constants in
  let count_bytes = Bytes.create 4 in
  Bytes.set_int32_le count_bytes 0 (Int32.of_int count);
  let total_const_size =
    Array.fold_left
      (fun acc const -> acc + Bytes.length (encode_constant const))
      0
      constants
  in
  let result = Bytes.create (4 + total_const_size) in
  Bytes.blit count_bytes 0 result 0 4;
  let _ =
    Array.fold_left
      (fun offset const ->
        let const_bytes = encode_constant const in
        let const_len = Bytes.length const_bytes in
        Bytes.blit const_bytes 0 result offset const_len;
        offset + const_len)
      4
      constants
  in
  result

let encode_program program =
  let header = Bytes.create 32 in
  Bytes.blit_string "MUSI" 0 header 0 4;
  Bytes.set_int16_le header 4 1;
  Bytes.set_int16_le header 6 0;
  let constant_bytes = encode_constants program.constants in
  let type_bytes = encode_record_types program.records in
  let proc_bytes = encode_procs program.procs in
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
