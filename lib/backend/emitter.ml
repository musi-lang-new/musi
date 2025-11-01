(** GIL bytecode emitter with Roslyn-style metadata.

    Two-pass emission:
    1. Build metadata tables from bound tree
    2. Emit bytecode using metadata lookups *)

(* ========================================
   TYPES
   ======================================== *)

type t = {
    interner : Interner.t
  ; resolver : Resolver.t [@warning "-69"] (* TODO: use this *)
  ; metadata : Metadata.t
  ; mutable constants : Instr.constant list
  ; mutable locals : (Interner.symbol, int) Hashtbl.t
       [@warning "-69"] (* TODO: use this *)
  ; mutable next_local : int
  ; mutable code : Instr.instr list
}

let create interner resolver =
  {
    interner
  ; resolver
  ; metadata = Metadata.create ()
  ; constants = []
  ; locals = Hashtbl.create 16
  ; next_local = 0
  ; code = []
  }

(* ========================================
   LOW-LEVEL PRIMITIVES
   ======================================== *)

let emit_instr t instr = t.code <- instr :: t.code

let add_constant t constant =
  match List.find_index (fun c -> c = constant) t.constants with
  | Some idx -> idx
  | None ->
    let idx = List.length t.constants in
    t.constants <- constant :: t.constants;
    idx

let alloc_local t name =
  match Hashtbl.find_opt t.locals name with
  | Some idx -> idx
  | None ->
    let idx = t.next_local in
    Hashtbl.add t.locals name idx;
    t.next_local <- t.next_local + 1;
    idx

let reset_locals t =
  Hashtbl.clear t.locals;
  t.next_local <- 0

let get_constant_pool t = List.rev t.constants

(* ========================================
   OPCODE EMISSION HELPERS
   ======================================== *)

let max_short_local = 256

let emit_ldloc t idx =
  match idx with
  | 0 -> emit_instr t Instr.Ldloc_0
  | 1 -> emit_instr t Instr.Ldloc_1
  | 2 -> emit_instr t Instr.Ldloc_2
  | 3 -> emit_instr t Instr.Ldloc_3
  | n when n < max_short_local -> emit_instr t (Instr.LdlocS n)
  | n -> emit_instr t (Instr.Ldloc n)

let emit_stloc t idx =
  match idx with
  | 0 -> emit_instr t Instr.Stloc_0
  | 1 -> emit_instr t Instr.Stloc_1
  | 2 -> emit_instr t Instr.Stloc_2
  | 3 -> emit_instr t Instr.Stloc_3
  | n when n < max_short_local -> emit_instr t (Instr.StlocS n)
  | n -> emit_instr t (Instr.Stloc n)

let emit_ldci4 t n =
  match n with
  | -1l -> emit_instr t Instr.LdcI4M1
  | 0l -> emit_instr t Instr.LdcI4_0
  | 1l -> emit_instr t Instr.LdcI4_1
  | 2l -> emit_instr t Instr.LdcI4_2
  | 3l -> emit_instr t Instr.LdcI4_3
  | 4l -> emit_instr t Instr.LdcI4_4
  | 5l -> emit_instr t Instr.LdcI4_5
  | 6l -> emit_instr t Instr.LdcI4_6
  | 7l -> emit_instr t Instr.LdcI4_7
  | 8l -> emit_instr t Instr.LdcI4_8
  | n when n >= -128l && n <= 127l ->
    emit_instr t (Instr.LdcI4S (Int32.to_int n))
  | n -> emit_instr t (Instr.LdcI4 n)

(* ========================================
   PASS 1: METADATA COLLECTION
   ======================================== *)

let rec collect_metadata_node t (node : Node.node) =
  match node.kind with
  | Node.ExprBinding { pat; init; _ } -> (
    collect_metadata_node t init;
    match (pat.kind, init.kind) with
    | Node.ExprIdent { name }, Node.ExprProc { params; external_; _ } ->
      let param_count = List.length params.items in
      let is_extern = Option.is_some external_ in
      ignore
        (Metadata.add_proc t.metadata name param_count 0 is_extern external_)
    | _ -> ())
  | Node.ExprTextLit { value } -> ignore (Metadata.add_string t.metadata value)
  | Node.ExprCall { callee; args } ->
    collect_metadata_node t callee;
    List.iter (collect_metadata_node t) args.items
  | Node.ExprBinary { lhs; rhs; _ } ->
    collect_metadata_node t lhs;
    collect_metadata_node t rhs
  | Node.ExprUnary { operand; _ } -> collect_metadata_node t operand
  | Node.ExprIf { cond; then_br; else_br } ->
    collect_metadata_node t cond;
    collect_metadata_node t then_br;
    Option.iter (collect_metadata_node t) else_br
  | Node.ExprBlock { body; _ } -> List.iter (collect_metadata_node t) body.items
  | Node.ExprProc { body; _ } -> Option.iter (collect_metadata_node t) body
  | _ -> ()

let collect_metadata t program = List.iter (collect_metadata_node t) program

(* ========================================
   PASS 2: BYTECODE EMISSION
   ======================================== *)

let rec emit_expr t (node : Node.node) =
  match node.kind with
  | Node.ExprIntLit { value; _ } ->
    let n = Int32.of_string value in
    emit_ldci4 t n
  | Node.ExprTextLit { value } ->
    let text = Interner.resolve t.interner value in
    let idx = add_constant t (Instr.CText text) in
    emit_instr t (Instr.Ldstr idx)
  | Node.ExprBoolLit { value } ->
    if value then emit_ldci4 t 1l else emit_ldci4 t 0l
  | Node.ExprUnitLit -> emit_instr t Instr.LdUnit
  | Node.ExprIdent { name } -> (
    match Resolver.lookup t.resolver name with
    | Some { kind = Resolver.SymAlias { target; _ }; _ } ->
      let idx = alloc_local t target in
      emit_ldloc t idx
    | _ ->
      let idx = alloc_local t name in
      emit_ldloc t idx)
  | Node.ExprBinary { op; lhs; rhs } -> (
    emit_expr t lhs;
    emit_expr t rhs;
    match op with
    | Token.Plus -> emit_instr t Instr.Add
    | Token.Minus -> emit_instr t Instr.Sub
    | Token.Star -> emit_instr t Instr.Mul
    | Token.Slash -> emit_instr t Instr.Div
    | Token.Eq -> emit_instr t Instr.Ceq
    | Token.Lt -> emit_instr t Instr.Clt
    | Token.Gt -> emit_instr t Instr.Cgt
    | _ -> ())
  | Node.ExprUnary { op; operand } -> (
    emit_expr t operand;
    match op with
    | Token.Minus -> emit_instr t Instr.Neg
    | Token.KwNot -> emit_instr t Instr.Not
    | _ -> ())
  | Node.ExprCall { callee; args } -> (
    List.iter (emit_expr t) args.items;
    match callee.kind with
    | Node.ExprIdent { name } -> (
      let target_name =
        match Resolver.lookup t.resolver name with
        | Some { kind = Resolver.SymAlias { target; _ }; _ } -> target
        | _ -> name
      in
      let procs = Metadata.all_procs t.metadata in
      match
        List.find_opt
          (fun (p : Metadata.proc_entry) -> p.name = target_name)
          procs
      with
      | Some proc ->
        if proc.bytecode_offset >= 0 then
          emit_instr t (Instr.Call proc.bytecode_offset)
      | None -> ())
    | _ -> ())
  | Node.ExprIf { cond; then_br; else_br } -> (
    emit_expr t cond;
    emit_instr t (Instr.BrfalseS 0);
    emit_expr t then_br;
    match else_br with
    | Some else_node ->
      emit_instr t (Instr.BrS 0);
      emit_expr t else_node
    | None -> ())
  | Node.ExprBlock { body; _ } -> (
    match body.items with
    | [] -> emit_instr t Instr.LdUnit
    | items ->
      List.iteri
        (fun i node ->
          emit_expr t node;
          if i < List.length items - 1 then emit_instr t Instr.Pop)
        items)
  | Node.ExprBinding { pat; init; _ } -> (
    emit_expr t init;
    match pat.kind with
    | Node.ExprIdent { name } ->
      let idx = alloc_local t name in
      emit_stloc t idx
    | _ -> ())
  | _ -> emit_instr t Instr.LdUnit

let emit_proc_body t (node : Node.node) =
  emit_expr t node;
  emit_instr t Instr.Ret

let emit_proc t name params body =
  reset_locals t;
  t.code <- [];
  List.iter (fun (p : Node.param) -> ignore (alloc_local t p.name)) params;
  let param_count = List.length params in
  (match body with
  | Some body_node -> emit_proc_body t body_node
  | None -> emit_instr t Instr.Ret);
  let code = List.rev t.code in
  let local_count = t.next_local - param_count in
  {
    Instr.name = Interner.resolve t.interner name
  ; param_count
  ; local_count
  ; code
  ; external_proc = false
  }

let emit_main_wrapper t program =
  reset_locals t;
  t.code <- [];
  let non_proc_nodes =
    List.filter
      (fun (node : Node.node) ->
        match node.kind with
        | Node.ExprBinding { init; _ } -> (
          match init.kind with Node.ExprProc _ -> false | _ -> true)
        | _ -> true)
      program
  in
  List.iteri
    (fun i node ->
      emit_expr t node;
      if i < List.length non_proc_nodes - 1 then emit_instr t Instr.Pop)
    non_proc_nodes;
  if List.length non_proc_nodes = 0 then emit_instr t Instr.LdUnit;
  emit_instr t Instr.Ret;
  let code = List.rev t.code in
  {
    Instr.name = "$main"
  ; param_count = 0
  ; local_count = t.next_local
  ; code
  ; external_proc = false
  }

let emit_extern_stub t name param_count =
  {
    Instr.name = Interner.resolve t.interner name
  ; param_count
  ; local_count = 0
  ; code = []
  ; external_proc = true
  }

let collect_proc_defs t program =
  let proc_defs = ref [] in
  List.iter
    (fun (node : Node.node) ->
      match node.kind with
      | Node.ExprBinding { pat; init; _ } -> (
        match (pat.kind, init.kind) with
        | Node.ExprIdent { name }, Node.ExprProc { params; body; external_; _ }
          ->
          let param_count = List.length params.items in
          if Option.is_some external_ then
            proc_defs := emit_extern_stub t name param_count :: !proc_defs
          else proc_defs := emit_proc t name params.items body :: !proc_defs
        | _ -> ())
      | _ -> ())
    program;
  List.rev !proc_defs

let fixup_proc_offsets t program =
  List.iteri
    (fun idx (node : Node.node) ->
      match node.kind with
      | Node.ExprBinding { pat; init; _ } -> (
        match (pat.kind, init.kind) with
        | Node.ExprIdent { name }, Node.ExprProc _ ->
          let procs = Metadata.all_procs t.metadata in
          let proc_entry =
            List.find_opt (fun (p : Metadata.proc_entry) -> p.name = name) procs
          in
          Option.iter
            (fun (entry : Metadata.proc_entry) ->
              entry.bytecode_offset <- idx + 1)
            proc_entry
        | _ -> ())
      | _ -> ())
    program

let emit_program_internal t program =
  collect_metadata t program;
  let proc_defs = collect_proc_defs t program in
  fixup_proc_offsets t program;
  let main_proc = emit_main_wrapper t program in
  let proc_list = main_proc :: proc_defs in
  {
    Instr.constants = Array.of_list (get_constant_pool t)
  ; procs = Array.of_list proc_list
  ; records = []
  }

(* ========================================
   PUBLIC API
   ======================================== *)

let emit_program t program = emit_program_internal t program

let emit_to_file t program filename =
  let bytecode_program = emit_program_internal t program in
  let encoded = Bytecode.encode_program bytecode_program in
  let oc = open_out_bin filename in
  output_bytes oc encoded;
  close_out oc
