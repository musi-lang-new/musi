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
