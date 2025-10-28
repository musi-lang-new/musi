(* ========================================
   TYPES
   ======================================== *)

type t = {
    interner : Musi_shared.Interner.t
  ; syms : Symbol.t
  ; diags : Musi_shared.Diagnostic.diagnostic_bag ref
  ; env : (Musi_shared.Interner.symbol, Types.ty) Hashtbl.t
  ; mutable expected_ret_ty : Types.ty option
}

(* ========================================
   CONSTRUCTORS
   ======================================== *)

let create interner syms =
  {
    interner
  ; syms
  ; diags = ref Musi_shared.Diagnostic.empty_bag
  ; env = Hashtbl.create 16
  ; expected_ret_ty = None
  }

(* ========================================
   ERROR REPORTING
   ======================================== *)

let error t msg span =
  t.diags :=
    Musi_shared.Diagnostic.add
      !(t.diags)
      (Musi_shared.Diagnostic.error msg span);
  Types.Error

let error_mismatch t expected actual span =
  let msg =
    Printf.sprintf
      "expected '%s', found '%s'"
      (Types.ty_to_string t.interner expected)
      (Types.ty_to_string t.interner actual)
  in
  error t msg span

(* ========================================
   TYPE RESOLUTION
   ======================================== *)

let rec resolve_ty t (ast_ty : Musi_syntax.Tree.ty) =
  match ast_ty.kind with
  | Musi_syntax.Tree.Named { name } -> resolve_named_ty t ast_ty name
  | Musi_syntax.Tree.Proc { param_tys; ret_ty } ->
    resolve_proc_ty t param_tys ret_ty
  | Musi_syntax.Tree.Optional _ | Musi_syntax.Tree.Fallible _ ->
    error t "optional/fallible types not yet supported" ast_ty.span
  | Musi_syntax.Tree.Ptr _ | Musi_syntax.Tree.Ref _ ->
    error t "pointer/reference types not yet supported" ast_ty.span
  | Musi_syntax.Tree.Infer | Musi_syntax.Tree.Array _ | Musi_syntax.Tree.Tuple _
  | Musi_syntax.Tree.Generic _ | Musi_syntax.Tree.Where _
  | Musi_syntax.Tree.ArrayRepeat _ | Musi_syntax.Tree.Error ->
    Types.Error

and resolve_named_ty t ast_ty name =
  match Symbol.lookup t.syms name with
  | Some { kind = Symbol.Type { fields = Some flds }; _ } ->
    let fields =
      List.map
        (fun (f : Musi_syntax.Tree.field) -> (f.name, resolve_ty t f.ty))
        flds
    in
    Types.Record { name; fields }
  | Some { kind = Symbol.Type { fields = None }; _ } ->
    Types.Record { name; fields = [] }
  | Some { kind = Symbol.Bind _; span; _ } ->
    resolve_error_ty t ast_ty name span "binding"
  | Some { kind = Symbol.Proc _; span; _ } ->
    resolve_error_ty t ast_ty name span "procedure"
  | None -> resolve_ty_builtin t ast_ty name

and resolve_error_ty t ast_ty name span context =
  let msg =
    Printf.sprintf
      "'%s' is %s, not type"
      (Musi_shared.Interner.to_string t.interner name)
      context
  in
  t.diags :=
    Musi_shared.Diagnostic.add
      !(t.diags)
      (Musi_shared.Diagnostic.error msg ast_ty.span);
  t.diags :=
    Musi_shared.Diagnostic.add
      !(t.diags)
      (Musi_shared.Diagnostic.note "defined here" span);
  Types.Error

and resolve_ty_builtin t ast_ty name =
  let name_str = Musi_shared.Interner.to_string t.interner name in
  match name_str with
  | "Int" -> Types.Int
  | "Nat" -> Types.Nat
  | "Bool" -> Types.Bool
  | "Text" -> Types.Text
  | "Unit" -> Types.Unit
  | _ -> error t (Printf.sprintf "unknown type name: '%s'" name_str) ast_ty.span

and resolve_proc_ty t params ret =
  let param_tys = List.map (resolve_ty t) params in
  let ret_ty = resolve_ty t ret in
  Types.Proc { params = param_tys; ret = ret_ty }

(* ========================================
   PATTERN BINDING
   ======================================== *)

let rec bind_pat_to_env t (pat : Musi_syntax.Tree.pat) ty =
  match pat.kind with
  | Musi_syntax.Tree.Ident { name } -> Hashtbl.add t.env name ty
  | Musi_syntax.Tree.Wildcard | Musi_syntax.Tree.IntLit _
  | Musi_syntax.Tree.BoolLit _ | Musi_syntax.Tree.TextLit _ ->
    ()
  | Musi_syntax.Tree.Tuple { pats }
  | Musi_syntax.Tree.Array { pats }
  | Musi_syntax.Tree.Or { pats } ->
    List.iter (fun p -> bind_pat_to_env t p ty) pats
  | Musi_syntax.Tree.Record { fields } ->
    List.iter (fun (_, p) -> bind_pat_to_env t p ty) fields
  | Musi_syntax.Tree.Range { start; end_; _ } ->
    bind_pat_to_env t start ty;
    bind_pat_to_env t end_ ty
  | Musi_syntax.Tree.Choice { pat = Some p; _ } -> bind_pat_to_env t p ty
  | Musi_syntax.Tree.Choice { pat = None; _ }
  | Musi_syntax.Tree.Rest _ | Musi_syntax.Tree.ValueBinding _
  | Musi_syntax.Tree.Error ->
    ()

(* ========================================
   EXPRESSION TYPE CHECKING
   ======================================== *)

let rec check_expr t expected (expr : Musi_syntax.Tree.expr) =
  match expr.kind with
  | Musi_syntax.Tree.IntLit _ ->
    if Types.equal_tys expected Types.Int || Types.equal_tys expected Types.Nat
    then expected
    else error_mismatch t expected Types.Nat expr.span
  | _ ->
    let actual = infer_expr t expr in
    if Types.equal_tys expected actual then expected
    else error_mismatch t expected actual expr.span

and infer_expr t (expr : Musi_syntax.Tree.expr) =
  match expr.kind with
  | Musi_syntax.Tree.IntLit _ -> Types.Nat
  | Musi_syntax.Tree.BinLit _ -> Types.Text
  | Musi_syntax.Tree.TextLit _ -> Types.Text
  | Musi_syntax.Tree.BoolLit _ -> Types.Bool
  | Musi_syntax.Tree.UnitLit -> Types.Unit
  | Musi_syntax.Tree.Ident { name } -> infer_ident_expr t expr name
  | Musi_syntax.Tree.Binary { op; lhs; rhs } ->
    infer_binary_expr t expr op lhs rhs
  | Musi_syntax.Tree.Unary { op; operand } -> infer_unary_expr t expr op operand
  | Musi_syntax.Tree.Call { callee; args } -> infer_call_expr t expr callee args
  | Musi_syntax.Tree.If { cond; then_br; else_br } ->
    infer_if_expr t expr cond then_br else_br
  | Musi_syntax.Tree.Block { stmts } -> infer_block_expr t stmts
  | Musi_syntax.Tree.Bind { pat; ty = Some ty_annot; init; _ } ->
    let expected_ty = resolve_ty t ty_annot in
    ignore (check_expr t expected_ty init);
    bind_pat_to_env t pat expected_ty;
    Types.Unit
  | Musi_syntax.Tree.Bind { pat; ty = None; init; _ } ->
    let init_ty = infer_expr t init in
    bind_pat_to_env t pat init_ty;
    Types.Unit
  | Musi_syntax.Tree.Cast { expr = inner; ty } ->
    ignore (infer_expr t inner);
    resolve_ty t ty
  | Musi_syntax.Tree.Proc { params; ret_ty; body } ->
    infer_proc_expr t expr params ret_ty body
  | Musi_syntax.Tree.Assign { lhs; rhs } -> infer_assign_expr t expr lhs rhs
  | Musi_syntax.Tree.Return { value } -> infer_return_expr t expr value
  | Musi_syntax.Tree.Match _ | Musi_syntax.Tree.Array _
  | Musi_syntax.Tree.Tuple _ | Musi_syntax.Tree.ArrayRepeat _
  | Musi_syntax.Tree.RecordLit _ | Musi_syntax.Tree.Record _
  | Musi_syntax.Tree.Choice _ | Musi_syntax.Tree.Interface _
  | Musi_syntax.Tree.Break _ | Musi_syntax.Tree.Continue
  | Musi_syntax.Tree.While _ | Musi_syntax.Tree.For _ | Musi_syntax.Tree.Field _
  | Musi_syntax.Tree.Index _ | Musi_syntax.Tree.Try _ | Musi_syntax.Tree.Defer _
  | Musi_syntax.Tree.Range _ | Musi_syntax.Tree.Async _
  | Musi_syntax.Tree.Await _ | Musi_syntax.Tree.Test _
  | Musi_syntax.Tree.Template _ ->
    error t "expression type checking not yet implemented" expr.span
  | Musi_syntax.Tree.Error -> Types.Error

and infer_ident_expr t expr name =
  match Hashtbl.find_opt t.env name with
  | Some ty -> ty
  | None ->
    error
      t
      (Printf.sprintf
         "undeclared identifier: '%s'"
         (Musi_shared.Interner.to_string t.interner name))
      expr.span

and infer_binary_expr t expr op lhs rhs =
  let lhs_ty = infer_expr t lhs in
  let rhs_ty = infer_expr t rhs in
  match op with
  | Musi_syntax.Token.Plus | Musi_syntax.Token.Minus | Musi_syntax.Token.Star
  | Musi_syntax.Token.Slash ->
    if
      Types.equal_tys lhs_ty rhs_ty
      && (Types.equal_tys lhs_ty Types.Int || Types.equal_tys lhs_ty Types.Nat)
    then lhs_ty
    else (
      ignore (error_mismatch t lhs_ty rhs_ty rhs.span);
      Types.Error)
  | Musi_syntax.Token.Eq | Musi_syntax.Token.Lt | Musi_syntax.Token.Gt
  | Musi_syntax.Token.LtEq | Musi_syntax.Token.GtEq ->
    if Types.equal_tys lhs_ty rhs_ty then Types.Bool
    else (
      ignore (error_mismatch t lhs_ty rhs_ty rhs.span);
      Types.Error)
  | _ ->
    error
      t
      (Printf.sprintf
         "unsupported binary operator: '%s'"
         (Musi_syntax.Token.kind_to_string t.interner op))
      expr.span

and infer_unary_expr t expr op operand =
  let operand_ty = infer_expr t operand in
  match op with
  | Musi_syntax.Token.Minus ->
    if
      Types.equal_tys operand_ty Types.Int
      || Types.equal_tys operand_ty Types.Nat
    then operand_ty
    else error_mismatch t Types.Int operand_ty operand.span
  | Musi_syntax.Token.KwNot ->
    if Types.equal_tys operand_ty Types.Bool then Types.Bool
    else error_mismatch t Types.Bool operand_ty operand.span
  | _ ->
    error
      t
      (Printf.sprintf
         "unsupported unary operator: '%s'"
         (Musi_syntax.Token.kind_to_string t.interner op))
      expr.span

and infer_call_expr t expr callee args =
  let callee_ty = infer_expr t callee in
  match callee_ty with
  | Types.Proc { params; ret } ->
    if List.length params <> List.length args then
      error
        t
        (Printf.sprintf
           "expected %d argument(s), found %d"
           (List.length params)
           (List.length args))
        expr.span
    else (
      List.iter2
        (fun param_ty arg -> ignore (check_expr t param_ty arg))
        params
        args;
      ret)
  | Types.Error -> Types.Error
  | _ -> error t "called object type is not procedure" callee.span

and infer_if_expr t _expr cond then_br else_br =
  let cond_ty = infer_expr t cond in
  if not (Types.equal_tys cond_ty Types.Bool) then
    ignore (error_mismatch t Types.Bool cond_ty cond.span);
  match else_br with
  | Some else_expr ->
    let then_ty = infer_expr t then_br in
    let else_ty = infer_expr t else_expr in
    if Types.equal_tys then_ty else_ty then then_ty
    else (
      ignore (error_mismatch t then_ty else_ty else_expr.span);
      Types.Error)
  | None ->
    ignore (infer_expr t then_br);
    Types.Unit

and infer_block_expr t stmts =
  match List.rev stmts with
  | [] -> Types.Unit
  | last :: rest -> (
    List.iter (check_stmt t) (List.rev rest);
    match last.Musi_syntax.Tree.kind with
    | Musi_syntax.Tree.Expr { expr } -> infer_expr t expr
    | _ ->
      check_stmt t last;
      Types.Unit)

and infer_proc_expr t _expr params ret_ty body =
  let param_tys =
    List.map (fun (p : Musi_syntax.Tree.param) -> resolve_ty t p.ty) params
  in
  let ret_type =
    match ret_ty with Some ty -> resolve_ty t ty | None -> Types.Unit
  in
  List.iter
    (fun (p : Musi_syntax.Tree.param) ->
      let ty = resolve_ty t p.ty in
      Hashtbl.add t.env p.name ty)
    params;
  let prev_ret_ty = t.expected_ret_ty in
  t.expected_ret_ty <- Some ret_type;
  (match body with
  | Some stmts ->
    let body_ty = infer_block_expr t stmts in
    if not (Types.equal_tys body_ty ret_type) then
      ignore (error_mismatch t ret_type body_ty (List.hd (List.rev stmts)).span)
  | None -> ());
  t.expected_ret_ty <- prev_ret_ty;
  List.iter
    (fun (p : Musi_syntax.Tree.param) -> Hashtbl.remove t.env p.name)
    params;
  Types.Proc { params = param_tys; ret = ret_type }

and infer_assign_expr t _expr lhs rhs =
  let lhs_ty = infer_expr t lhs in
  let rhs_ty = infer_expr t rhs in
  if not (Types.equal_tys lhs_ty rhs_ty) then
    ignore (error_mismatch t lhs_ty rhs_ty rhs.span);
  Types.Unit

and infer_return_expr t expr value =
  match t.expected_ret_ty with
  | None -> error t "'return' outside of procedure" expr.span
  | Some expected -> (
    match value with
    | Some v ->
      let actual = infer_expr t v in
      if not (Types.equal_tys expected actual) then
        ignore (error_mismatch t expected actual v.span);
      Types.Unit
    | None ->
      if not (Types.equal_tys expected Types.Unit) then
        ignore (error_mismatch t expected Types.Unit expr.span);
      Types.Unit)

(* ========================================
   STATEMENT TYPE CHECKING
   ======================================== *)

and check_stmt t (stmt : Musi_syntax.Tree.stmt) =
  match stmt.kind with
  | Musi_syntax.Tree.Expr { expr } -> ignore (infer_expr t expr)
  | Musi_syntax.Tree.Import _ | Musi_syntax.Tree.Export _
  | Musi_syntax.Tree.Alias _ ->
    ()
  | Musi_syntax.Tree.Error -> ()

(* ========================================
   PUBLIC API
   ======================================== *)

let check_program t program =
  List.iter (check_stmt t) program;
  !(t.diags)
