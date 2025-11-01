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
  ; external_proc : bool
}

type record_type_def = { name : string; fields : (string * string) list }

type program = {
    constants : constant array
  ; procs : proc_def array
  ; records : record_type_def list
}

let show_instr = function
  | Nop -> "nop"
  | LdcI4M1 -> "ldc.i4.m1"
  | LdcI4_0 -> "ldc.i4.0"
  | LdcI4_1 -> "ldc.i4.1"
  | LdcI4_2 -> "ldc.i4.2"
  | LdcI4_3 -> "ldc.i4.3"
  | LdcI4_4 -> "ldc.i4.4"
  | LdcI4_5 -> "ldc.i4.5"
  | LdcI4_6 -> "ldc.i4.6"
  | LdcI4_7 -> "ldc.i4.7"
  | LdcI4_8 -> "ldc.i4.8"
  | LdcI4S n -> Printf.sprintf "ldc.i4.s %d" n
  | LdcI4 n -> Printf.sprintf "ldc.i4 %ld" n
  | LdcI8 n -> Printf.sprintf "ldc.i8 %Ld" n
  | LdcR4 f -> Printf.sprintf "ldc.r4 %f" f
  | LdcR8 f -> Printf.sprintf "ldc.r8 %f" f
  | Ldstr idx -> Printf.sprintf "ldstr [%d]" idx
  | LdUnit -> "ldunit"
  | Dup -> "dup"
  | Pop -> "pop"
  | Ldloc_0 -> "ldloc.0"
  | Ldloc_1 -> "ldloc.1"
  | Ldloc_2 -> "ldloc.2"
  | Ldloc_3 -> "ldloc.3"
  | LdlocS n -> Printf.sprintf "ldloc.s %d" n
  | Ldloc n -> Printf.sprintf "ldloc %d" n
  | Stloc_0 -> "stloc.0"
  | Stloc_1 -> "stloc.1"
  | Stloc_2 -> "stloc.2"
  | Stloc_3 -> "stloc.3"
  | StlocS n -> Printf.sprintf "stloc.s %d" n
  | Stloc n -> Printf.sprintf "stloc %d" n
  | Ldarg_0 -> "ldarg.0"
  | Ldarg_1 -> "ldarg.1"
  | Ldarg_2 -> "ldarg.2"
  | Ldarg_3 -> "ldarg.3"
  | LdargS n -> Printf.sprintf "ldarg.s %d" n
  | Ldarg n -> Printf.sprintf "ldarg %d" n
  | Starg_0 -> "starg.0"
  | Starg_1 -> "starg.1"
  | Starg_2 -> "starg.2"
  | Starg_3 -> "starg.3"
  | StargS n -> Printf.sprintf "starg.s %d" n
  | Starg n -> Printf.sprintf "starg %d" n
  | BrS offset -> Printf.sprintf "br.s %d" offset
  | Br offset -> Printf.sprintf "br %ld" offset
  | BrfalseS offset -> Printf.sprintf "brfalse.s %d" offset
  | Brfalse offset -> Printf.sprintf "brfalse %ld" offset
  | BrtrueS offset -> Printf.sprintf "brtrue.s %d" offset
  | Brtrue offset -> Printf.sprintf "brtrue %ld" offset
  | Beq offset -> Printf.sprintf "beq %ld" offset
  | Bne offset -> Printf.sprintf "bne %ld" offset
  | Blt offset -> Printf.sprintf "blt %ld" offset
  | Ble offset -> Printf.sprintf "ble %ld" offset
  | Bgt offset -> Printf.sprintf "bgt %ld" offset
  | Bge offset -> Printf.sprintf "bge %ld" offset
  | Switch _ -> "switch"
  | Add -> "add"
  | AddOvf -> "add.ovf"
  | Sub -> "sub"
  | SubOvf -> "sub.ovf"
  | Mul -> "mul"
  | MulOvf -> "mul.ovf"
  | Div -> "div"
  | Mod -> "mod"
  | Pow -> "pow"
  | Neg -> "neg"
  | Ceq -> "ceq"
  | Cgt -> "cgt"
  | Clt -> "clt"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Not -> "not"
  | Shl -> "shl"
  | Shr -> "shr"
  | ConvI1 -> "conv.i1"
  | ConvI2 -> "conv.i2"
  | ConvI4 -> "conv.i4"
  | ConvI8 -> "conv.i8"
  | ConvN1 -> "conv.n1"
  | ConvN2 -> "conv.n2"
  | ConvN4 -> "conv.n4"
  | ConvN8 -> "conv.n8"
  | ConvR4 -> "conv.r4"
  | ConvR8 -> "conv.r8"
  | Newobj token -> Printf.sprintf "newobj [%d]" token
  | Newarr token -> Printf.sprintf "newarr [%d]" token
  | Ldfld token -> Printf.sprintf "ldfld [%d]" token
  | Stfld token -> Printf.sprintf "stfld [%d]" token
  | Ldelem token -> Printf.sprintf "ldelem [%d]" token
  | Stelem token -> Printf.sprintf "stelem [%d]" token
  | Ldlen -> "ldlen"
  | Box token -> Printf.sprintf "box [%d]" token
  | Unbox token -> Printf.sprintf "unbox [%d]" token
  | LdindI1 -> "ldind.i1"
  | LdindI2 -> "ldind.i2"
  | LdindI4 -> "ldind.i4"
  | LdindI8 -> "ldind.i8"
  | LdindN1 -> "ldind.n1"
  | LdindN2 -> "ldind.n2"
  | LdindN4 -> "ldind.n4"
  | LdindN8 -> "ldind.n8"
  | LdindR4 -> "ldind.r4"
  | LdindR8 -> "ldind.r8"
  | LdindRef -> "ldind.ref"
  | StindI1 -> "stind.i1"
  | StindI2 -> "stind.i2"
  | StindI4 -> "stind.i4"
  | StindI8 -> "stind.i8"
  | StindR4 -> "stind.r4"
  | StindR8 -> "stind.r8"
  | StindRef -> "stind.ref"
  | Ldloca n -> Printf.sprintf "ldloca %d" n
  | Ldarga n -> Printf.sprintf "ldarga %d" n
  | Ldflda token -> Printf.sprintf "ldflda [%d]" token
  | Ldelema token -> Printf.sprintf "ldelema [%d]" token
  | Sizeof token -> Printf.sprintf "sizeof [%d]" token
  | Localloc -> "localloc"
  | Isinst token -> Printf.sprintf "isinst [%d]" token
  | Castclass token -> Printf.sprintf "castclass [%d]" token
  | Ldtoken token -> Printf.sprintf "ldtoken [%d]" token
  | Ldnull -> "ldnull"
  | Brnull offset -> Printf.sprintf "brnull %ld" offset
  | Brnonnull offset -> Printf.sprintf "brnonnull %ld" offset
  | Call proc_id -> Printf.sprintf "call [%d]" proc_id
  | Calli token -> Printf.sprintf "calli [%d]" token
  | Callvirt token -> Printf.sprintf "callvirt [%d]" token
  | Ret -> "ret"
  | Jmp proc_id -> Printf.sprintf "jmp [%d]" proc_id
  | Throw -> "throw"
  | Rethrow -> "rethrow"
  | Leave offset -> Printf.sprintf "leave %ld" offset
  | Defer proc_id -> Printf.sprintf "defer [%d]" proc_id
  | GcRetain -> "gc.retain"
  | GcRelease -> "gc.release"
  | GcAutorelease -> "gc.autorelease"
