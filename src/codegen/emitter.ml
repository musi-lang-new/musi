(* ========================================
   TYPES
   ======================================== *)

type jump_patch = { position : int }

type t = {
    interner : Musi_shared.Interner.t
  ; mutable constants : Instr.constant list
  ; mutable locals : (Musi_shared.Interner.symbol, int) Hashtbl.t
  ; mutable next_local : int
  ; mutable code : Instr.instr list
  ; mutable pending_jumps : jump_patch list
}

let create interner =
  {
    interner
  ; constants = []
  ; locals = Hashtbl.create 16
  ; next_local = 0
  ; code = []
  ; pending_jumps = []
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
  | Musi_syntax.Token.Plus -> emit_instr t Instr.Add
  | Musi_syntax.Token.Minus -> emit_instr t Instr.Sub
  | Musi_syntax.Token.Star -> emit_instr t Instr.Mul
  | Musi_syntax.Token.Slash -> emit_instr t Instr.Div
  | Musi_syntax.Token.Eq -> emit_instr t Instr.Ceq
  | Musi_syntax.Token.Lt -> emit_instr t Instr.Clt
  | Musi_syntax.Token.Gt -> emit_instr t Instr.Cgt
  | _ -> ()

and emit_unary_expr t op operand =
  emit_expr t operand;
  match op with
  | Musi_syntax.Token.Minus -> emit_instr t Instr.Neg
  | Musi_syntax.Token.KwNot -> emit_instr t Instr.Not
  | _ -> ()

and emit_call_expr t (callee : Musi_syntax.Tree.expr) args =
  List.iter (emit_expr t) args;
  match callee.kind with
  | Musi_syntax.Tree.Ident { name = _ } ->
    let proc_id = 0 in
    emit_instr t (Instr.Call proc_id)
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

and emit_block_expr t (stmts : Musi_syntax.Tree.stmt list) =
  match stmts with
  | [] -> emit_instr t Instr.LdUnit
  | [ single_stmt ] -> (
    match single_stmt.kind with
    | Musi_syntax.Tree.Expr { expr } -> emit_expr t expr
    | _ -> emit_instr t Instr.LdUnit)
  | _ -> emit_instr t Instr.LdUnit

and emit_expr t (expr : Musi_syntax.Tree.expr) =
  match expr.kind with
  | Musi_syntax.Tree.IntLit { value; suffix = _ } ->
    let n = Int32.of_string value in
    emit_ldci4 t n
  | Musi_syntax.Tree.TextLit { value } ->
    let text = Musi_shared.Interner.to_string t.interner value in
    let idx = add_constant t (Instr.CText text) in
    emit_instr t (Instr.Ldstr idx)
  | Musi_syntax.Tree.BoolLit { value } ->
    if value then emit_ldci4 t 1l else emit_ldci4 t 0l
  | Musi_syntax.Tree.Ident { name } ->
    let idx = alloc_local t name in
    emit_ldloc t idx
  | Musi_syntax.Tree.Binary { op; lhs; rhs } -> emit_binary_expr t op lhs rhs
  | Musi_syntax.Tree.Unary { op; operand } -> emit_unary_expr t op operand
  | Musi_syntax.Tree.Call { callee; args } -> emit_call_expr t callee args
  | Musi_syntax.Tree.If { cond; then_br; else_br } ->
    emit_if_expr t cond then_br else_br
  | Musi_syntax.Tree.Block { stmts } -> emit_block_expr t stmts
  | Musi_syntax.Tree.Bind { pat; init; _ } -> (
    emit_expr t init;
    match pat.kind with
    | Musi_syntax.Tree.Ident { name } ->
      let idx = alloc_local t name in
      emit_stloc t idx
    | _ -> ())
  | Musi_syntax.Tree.UnitLit | Musi_syntax.Tree.Match _
  | Musi_syntax.Tree.Array _ | Musi_syntax.Tree.Tuple _
  | Musi_syntax.Tree.Field _ | Musi_syntax.Tree.Index _ | Musi_syntax.Tree.Try _
  | Musi_syntax.Tree.Defer _ | Musi_syntax.Tree.Range _
  | Musi_syntax.Tree.Async _ | Musi_syntax.Tree.Await _
  | Musi_syntax.Tree.Cast _ | Musi_syntax.Tree.Test _
  | Musi_syntax.Tree.Template _ | Musi_syntax.Tree.BinLit _
  | Musi_syntax.Tree.RecordLit _ | Musi_syntax.Tree.Record _
  | Musi_syntax.Tree.Choice _ | Musi_syntax.Tree.Interface _
  | Musi_syntax.Tree.Proc _ | Musi_syntax.Tree.Assign _
  | Musi_syntax.Tree.Return _ | Musi_syntax.Tree.Break _
  | Musi_syntax.Tree.Continue | Musi_syntax.Tree.While _
  | Musi_syntax.Tree.For _ | Musi_syntax.Tree.ArrayRepeat _
  | Musi_syntax.Tree.Error ->
    ()

(* ========================================
   STATEMENT EMISSION
   ======================================== *)

let emit_stmt t (stmt : Musi_syntax.Tree.stmt) =
  match stmt.kind with
  | Musi_syntax.Tree.Expr { expr } -> emit_expr t expr
  | Musi_syntax.Tree.Import _ | Musi_syntax.Tree.Export _
  | Musi_syntax.Tree.Alias _ | Musi_syntax.Tree.Error ->
    ()

(* ========================================
   PROCEDURE EMISSION
   ======================================== *)

let emit_proc_params t params =
  List.iter
    (fun (param : Musi_syntax.Tree.param) -> ignore (alloc_local t param.name))
    params

let emit_proc_body t body =
  List.iter (emit_stmt t) body;
  emit_instr t Instr.Ret

let finalize_proc_code t param_count =
  let proc_code = List.rev t.code in
  let local_count = t.next_local - param_count in
  (param_count, local_count, proc_code)

let _emit_proc t _name params _ret_ty body =
  emit_proc_params t params;
  let param_count = List.length params in
  emit_proc_body t body;
  finalize_proc_code t param_count

(* ========================================
   PROGRAM EMISSION
   ======================================== *)

let emit_program_internal t program =
  reset_locals t;
  t.code <- [];
  List.iter (emit_stmt t) program;
  { Instr.constants = get_constant_pool t; procs = []; records = [] }

(* ========================================
   PUBLIC API
   ======================================== *)

let emit_program t program = emit_program_internal t program

let emit_to_file t program filename =
  let bytecode_program = emit_program_internal t program in
  let encoded = Instr.encode_program bytecode_program in
  let oc = open_out_bin filename in
  output_bytes oc encoded;
  close_out oc
