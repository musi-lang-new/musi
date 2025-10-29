(* ========================================
   TYPES
   ======================================== *)

type jump_patch = { position : int }

type t = {
    interner : Interner.t
  ; mutable constants : Instr.constant list
  ; mutable locals : (Interner.symbol, int) Hashtbl.t
  ; mutable next_local : int
  ; mutable code : Instr.instr list
  ; mutable pending_jumps : jump_patch list
  ; mutable procs : Instr.proc_def list
  ; proc_map : (Interner.symbol, int) Hashtbl.t
  ; binder_syms : Symbol.t
}

let create interner binder_syms =
  {
    interner
  ; constants = []
  ; locals = Hashtbl.create 16
  ; next_local = 0
  ; code = []
  ; pending_jumps = []
  ; procs = []
  ; proc_map = Hashtbl.create 16
  ; binder_syms
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

(* ========================================
   STATE MANAGEMENT
   ======================================== *)

let reset_locals t =
  Hashtbl.clear t.locals;
  t.locals <- Hashtbl.create 16;
  t.next_local <- 0

let get_current_position t = List.length t.code
let get_constant_pool t = List.rev t.constants

(* ========================================
   JUMP PATCHING
   ======================================== *)

let emit_jump_placeholder t jump_instr =
  let position = List.length t.code in
  emit_instr t jump_instr;
  t.pending_jumps <- { position } :: t.pending_jumps

let patch_jump t jump_position target_position =
  let offset = target_position - jump_position - 1 in
  let patched_jump =
    match List.nth t.code jump_position with
    | Instr.BrfalseS _ -> Instr.BrfalseS offset
    | Instr.Brfalse _ -> Instr.Brfalse (Int32.of_int offset)
    | Instr.BrS _ -> Instr.BrS offset
    | Instr.Br _ -> Instr.Br (Int32.of_int offset)
    | instr -> instr
  in
  t.code <-
    List.mapi
      (fun pos instr -> if pos = jump_position then patched_jump else instr)
      t.code

let resolve_all_pending_jumps t target_position =
  List.iter
    (fun jump -> patch_jump t jump.position target_position)
    t.pending_jumps;
  t.pending_jumps <- []

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
   EXPRESSION EMISSION
   ======================================== *)

let rec emit_binary_expr t op lhs rhs =
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
  | _ -> ()

and emit_unary_expr t op operand =
  emit_expr t operand;
  match op with
  | Token.Minus -> emit_instr t Instr.Neg
  | Token.KwNot -> emit_instr t Instr.Not
  | _ -> ()

and emit_call_expr t (callee : Tree.expr) args _binder_syms =
  List.iter (emit_expr t) args;
  match callee.kind with
  | Tree.Ident { name } -> (
    match Hashtbl.find_opt t.proc_map name with
    | Some proc_id -> emit_instr t (Instr.Call proc_id)
    | None -> ())
  | _ -> ()

and emit_if_expr t cond then_br else_br =
  emit_expr t cond;
  emit_jump_placeholder t (Instr.BrfalseS 0);
  emit_expr t then_br;
  match else_br with
  | Some else_expr ->
    emit_jump_placeholder t (Instr.BrS 0);
    let else_start = get_current_position t in
    resolve_all_pending_jumps t else_start;
    emit_expr t else_expr;
    let if_end = get_current_position t in
    resolve_all_pending_jumps t if_end
  | None ->
    let if_end = get_current_position t in
    resolve_all_pending_jumps t if_end

and emit_block_expr t (stmts : Tree.stmt list) =
  match stmts with
  | [] -> emit_instr t Instr.LdUnit
  | [ single_stmt ] -> (
    match single_stmt.kind with
    | Tree.Expr { expr } -> emit_expr t expr
    | _ -> emit_instr t Instr.LdUnit)
  | _ -> emit_instr t Instr.LdUnit

and emit_expr t (expr : Tree.expr) =
  match expr.kind with
  | Tree.IntLit { value; suffix = _ } ->
    let n = Int32.of_string value in
    emit_ldci4 t n
  | Tree.TextLit { value } ->
    let text = Interner.to_string t.interner value in
    let idx = add_constant t (Instr.CText text) in
    emit_instr t (Instr.Ldstr idx)
  | Tree.BoolLit { value } -> if value then emit_ldci4 t 1l else emit_ldci4 t 0l
  | Tree.Ident { name } ->
    let idx = alloc_local t name in
    emit_ldloc t idx
  | Tree.Binary { op; lhs; rhs } -> emit_binary_expr t op lhs rhs
  | Tree.Unary { op; operand } -> emit_unary_expr t op operand
  | Tree.Call { callee; args } -> emit_call_expr t callee args t.binder_syms
  | Tree.If { cond; then_br; else_br } -> emit_if_expr t cond then_br else_br
  | Tree.Block { stmts } -> emit_block_expr t stmts
  | Tree.Bind { pat; init; _ } -> (
    emit_expr t init;
    match pat.kind with
    | Tree.Ident { name } ->
      let idx = alloc_local t name in
      emit_stloc t idx
    | _ -> ())
  | Tree.UnitLit | Tree.Match _ | Tree.Array _ | Tree.Tuple _ | Tree.Field _
  | Tree.Index _ | Tree.Try _ | Tree.Defer _ | Tree.Range _ | Tree.Async _
  | Tree.Await _ | Tree.Cast _ | Tree.Test _ | Tree.Template _ | Tree.BinLit _
  | Tree.RecordLit _ | Tree.Record _ | Tree.Choice _ | Tree.Interface _
  | Tree.Proc _ | Tree.Assign _ | Tree.Return _ | Tree.Break _ | Tree.Continue
  | Tree.While _ | Tree.For _ | Tree.ArrayRepeat _ | Tree.Error ->
    ()

(* ========================================
   STATEMENT EMISSION
   ======================================== *)

let emit_stmt t (stmt : Tree.stmt) =
  match stmt.kind with
  | Tree.Expr { expr } -> emit_expr t expr
  | Tree.Import _ | Tree.Export _ | Tree.Alias _ | Tree.Error -> ()

(* ========================================
   PROCEDURE EMISSION
   ======================================== *)

let emit_proc_params t params =
  List.iter
    (fun (param : Tree.param) -> ignore (alloc_local t param.name))
    params

let emit_proc_body t body =
  List.iter (emit_stmt t) body;
  emit_instr t Instr.Ret

let finalize_proc_code t param_count =
  let proc_code = List.rev t.code in
  let local_count = t.next_local - param_count in
  (param_count, local_count, proc_code)

let emit_proc t name params body =
  reset_locals t;
  t.code <- [];
  emit_proc_params t params;
  let param_count = List.length params in
  (match body with
  | Some stmts -> emit_proc_body t stmts
  | None -> emit_instr t Instr.Ret);
  let param_count, local_count, code = finalize_proc_code t param_count in
  let proc_def =
    {
      Instr.name = Interner.to_string t.interner name
    ; param_count
    ; local_count
    ; code
    ; external_proc = false
    }
  in
  let proc_id = List.length t.procs in
  t.procs <- proc_def :: t.procs;
  Hashtbl.add t.proc_map name proc_id

(* ========================================
   PROGRAM EMISSION
   ======================================== *)

let collect_extern_procs t =
  Symbol.iter_all t.binder_syms (fun sym ->
    match sym.Symbol.kind with
    | Symbol.Extern { lib_name; _ } when lib_name = "intrinsic" ->
      let name_str = Interner.to_string t.interner sym.name in
      let proc_def =
        {
          Instr.name = name_str
        ; param_count = 1
        ; local_count = 0
        ; code = []
        ; external_proc = true
        }
      in
      let proc_id = List.length t.procs in
      t.procs <- proc_def :: t.procs;
      Hashtbl.add t.proc_map sym.name proc_id
    | _ -> ())

let collect_procs t program =
  collect_extern_procs t;
  List.iter
    (fun (stmt : Tree.stmt) ->
      match stmt.kind with
      | Tree.Expr { expr } -> (
        match expr.kind with
        | Tree.Bind { pat; init; _ } -> (
          match (pat.kind, init.kind) with
          | Tree.Ident { name }, Tree.Proc { params; body; _ } ->
            emit_proc t name params body
          | _ -> ())
        | _ -> ())
      | _ -> ())
    program

let emit_main_wrapper t program =
  reset_locals t;
  t.code <- [];
  let non_proc_stmts =
    List.filter
      (fun (stmt : Tree.stmt) ->
        match stmt.kind with
        | Tree.Expr { expr } -> (
          match expr.kind with
          | Tree.Bind { init; _ } -> (
            match init.kind with Tree.Proc _ -> false | _ -> true)
          | _ -> true)
        | _ -> true)
      program
  in
  List.iter (emit_stmt t) non_proc_stmts;
  emit_instr t Instr.Ret;
  let code = List.rev t.code in
  let local_count = t.next_local in
  {
    Instr.name = "$main"
  ; param_count = 0
  ; local_count
  ; code
  ; external_proc = false
  }

let emit_program_internal t program =
  t.procs <- [];
  Hashtbl.clear t.proc_map;
  collect_procs t program;
  let main_proc = emit_main_wrapper t program in
  let const_list = get_constant_pool t in
  let proc_list = main_proc :: List.rev t.procs in
  {
    Instr.constants = Array.of_list const_list
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
