(* ========================================
   TYPES
   ======================================== *)

type t = {
    interner : Interner.t
  ; syms : Symbol.t
  ; diags : Diagnostic.diagnostic_bag ref
  ; env : (Interner.symbol, Types.ty) Hashtbl.t
  ; mutable expected_ret_ty : Types.ty option
}

(* ========================================
   CONSTRUCTORS
   ======================================== *)

let create interner syms =
  {
    interner
  ; syms
  ; diags = ref Diagnostic.empty_bag
  ; env = Hashtbl.create 16
  ; expected_ret_ty = None
  }

(* ========================================
   ERROR REPORTING
   ======================================== *)

let error t msg span =
  t.diags :=
    Diagnostic.add
      !(t.diags)
      (Diagnostic.error msg span);
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

let rec resolve_ty t (ast_ty : Tree.ty) =
  match ast_ty.kind with
  | Tree.Named { name } -> resolve_named_ty t ast_ty name
  | Tree.Proc { param_tys; ret_ty } ->
    resolve_proc_ty t param_tys ret_ty
  | Tree.Optional _ | Tree.Fallible _ ->
    error t "optional/fallible types not yet supported" ast_ty.span
  | Tree.Ptr _ | Tree.Ref _ ->
    error t "pointer/reference types not yet supported" ast_ty.span
  | Tree.Infer | Tree.Array _ | Tree.Tuple _
  | Tree.Generic _ | Tree.Where _
  | Tree.ArrayRepeat _ | Tree.Error ->
    Types.Error

and resolve_named_ty t ast_ty name =
  match Symbol.lookup t.syms name with
  | Some { kind = Symbol.Type { fields = Some flds }; _ } ->
    let fields =
      List.map
        (fun (f : Tree.field) -> (f.name, resolve_ty t f.ty))
        flds
    in
    Types.Record { name; fields }
  | Some { kind = Symbol.Type { fields = None }; _ } ->
    Types.Record { name; fields = [] }
  | Some { kind = Symbol.Bind _; span; _ } ->
    resolve_error_ty t ast_ty name span "binding"
  | Some { kind = Symbol.Proc _; span; _ } ->
    resolve_error_ty t ast_ty name span "procedure"
  | Some { kind = Symbol.Extern _; span; _ } ->
    resolve_error_ty t ast_ty name span "extern procedure"
  | None -> resolve_ty_builtin t ast_ty name

and resolve_error_ty t ast_ty name span context =
  let msg =
    Printf.sprintf
      "'%s' is %s, not type"
      (Interner.to_string t.interner name)
      context
  in
  t.diags :=
    Diagnostic.add
      !(t.diags)
      (Diagnostic.error msg ast_ty.span);
  t.diags :=
    Diagnostic.add
      !(t.diags)
      (Diagnostic.note "defined here" span);
  Types.Error

and resolve_ty_builtin t ast_ty name =
  let name_str = Interner.to_string t.interner name in
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

let rec bind_pat_to_env t (pat : Tree.pat) ty =
  match pat.kind with
  | Tree.Ident { name } -> Hashtbl.add t.env name ty
  | Tree.Wildcard | Tree.IntLit _
  | Tree.BoolLit _ | Tree.TextLit _ ->
    ()
  | Tree.Tuple { pats }
  | Tree.Array { pats }
  | Tree.Or { pats } ->
    List.iter (fun p -> bind_pat_to_env t p ty) pats
  | Tree.Record { fields } ->
    List.iter (fun (_, p) -> bind_pat_to_env t p ty) fields
  | Tree.Range { start; end_; _ } ->
    bind_pat_to_env t start ty;
    bind_pat_to_env t end_ ty
  | Tree.Choice { pat = Some p; _ } -> bind_pat_to_env t p ty
  | Tree.Choice { pat = None; _ }
  | Tree.Rest _ | Tree.ValueBinding _
  | Tree.Error ->
    ()

(* ========================================
   EXPRESSION TYPE CHECKING
   ======================================== *)

let rec check_expr t expected (expr : Tree.expr) =
  match expr.kind with
  | Tree.IntLit _ ->
    if Types.equal_tys expected Types.Int || Types.equal_tys expected Types.Nat
    then expected
    else error_mismatch t expected Types.Nat expr.span
  | _ ->
    let actual = infer_expr t expr in
    if Types.equal_tys expected actual then expected
    else error_mismatch t expected actual expr.span

and infer_expr t (expr : Tree.expr) =
  match expr.kind with
  | Tree.IntLit _ -> Types.Nat
  | Tree.BinLit _ -> Types.Text
  | Tree.TextLit _ -> Types.Text
  | Tree.BoolLit _ -> Types.Bool
  | Tree.UnitLit -> Types.Unit
  | Tree.Ident { name } -> infer_ident_expr t expr name
  | Tree.Binary { op; lhs; rhs } ->
    infer_binary_expr t expr op lhs rhs
  | Tree.Unary { op; operand } -> infer_unary_expr t expr op operand
  | Tree.Call { callee; args } -> infer_call_expr t expr callee args
  | Tree.If { cond; then_br; else_br } ->
    infer_if_expr t expr cond then_br else_br
  | Tree.Block { stmts } -> infer_block_expr t stmts
  | Tree.Bind { pat; ty = Some ty_annot; init; _ } ->
    let expected_ty = resolve_ty t ty_annot in
    ignore (check_expr t expected_ty init);
    bind_pat_to_env t pat expected_ty;
    Types.Unit
  | Tree.Bind { pat; ty = None; init; _ } ->
    let init_ty = infer_expr t init in
    bind_pat_to_env t pat init_ty;
    Types.Unit
  | Tree.Cast { expr = inner; ty } ->
    ignore (infer_expr t inner);
    resolve_ty t ty
  | Tree.Proc { params; ret_ty; body } ->
    infer_proc_expr t expr params ret_ty body
  | Tree.Assign { lhs; rhs } -> infer_assign_expr t expr lhs rhs
  | Tree.Return { value } -> infer_return_expr t expr value
  | Tree.Match _ | Tree.Array _
  | Tree.Tuple _ | Tree.ArrayRepeat _
  | Tree.RecordLit _ | Tree.Record _
  | Tree.Choice _ | Tree.Interface _
  | Tree.Break _ | Tree.Continue
  | Tree.While _ | Tree.For _ | Tree.Field _
  | Tree.Index _ | Tree.Try _ | Tree.Defer _
  | Tree.Range _ | Tree.Async _
  | Tree.Await _ | Tree.Test _
  | Tree.Template _ ->
    error t "expression type checking not yet implemented" expr.span
  | Tree.Error -> Types.Error

and infer_ident_expr t _expr _name =
  match Hashtbl.find_opt t.env _name with
  | Some ty -> ty
  | None ->
    (* Binder already reported undeclared identifiers *)
    Types.Error

and infer_binary_expr t expr op lhs rhs =
  let lhs_ty = infer_expr t lhs in
  let rhs_ty = infer_expr t rhs in
  match op with
  | Token.Plus | Token.Minus | Token.Star
  | Token.Slash ->
    if
      Types.equal_tys lhs_ty rhs_ty
      && (Types.equal_tys lhs_ty Types.Int || Types.equal_tys lhs_ty Types.Nat)
    then lhs_ty
    else (
      ignore (error_mismatch t lhs_ty rhs_ty rhs.span);
      Types.Error)
  | Token.Eq | Token.Lt | Token.Gt
  | Token.LtEq | Token.GtEq ->
    if Types.equal_tys lhs_ty rhs_ty then Types.Bool
    else (
      ignore (error_mismatch t lhs_ty rhs_ty rhs.span);
      Types.Error)
  | _ ->
    error
      t
      (Printf.sprintf
         "unsupported binary operator: '%s'"
         (Token.kind_to_string t.interner op))
      expr.span

and infer_unary_expr t expr op operand =
  let operand_ty = infer_expr t operand in
  match op with
  | Token.Minus ->
    if
      Types.equal_tys operand_ty Types.Int
      || Types.equal_tys operand_ty Types.Nat
    then operand_ty
    else error_mismatch t Types.Int operand_ty operand.span
  | Token.KwNot ->
    if Types.equal_tys operand_ty Types.Bool then Types.Bool
    else error_mismatch t Types.Bool operand_ty operand.span
  | _ ->
    error
      t
      (Printf.sprintf
         "unsupported unary operator: '%s'"
         (Token.kind_to_string t.interner op))
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
    match last.Tree.kind with
    | Tree.Expr { expr } -> infer_expr t expr
    | _ ->
      check_stmt t last;
      Types.Unit)

and infer_proc_expr t _expr params ret_ty body =
  let param_tys =
    List.map (fun (p : Tree.param) -> resolve_ty t p.ty) params
  in
  let ret_type =
    match ret_ty with Some ty -> resolve_ty t ty | None -> Types.Unit
  in
  List.iter
    (fun (p : Tree.param) ->
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
    (fun (p : Tree.param) -> Hashtbl.remove t.env p.name)
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

and check_stmt t (stmt : Tree.stmt) =
  match stmt.kind with
  | Tree.Expr { expr } -> ignore (infer_expr t expr)
  | Tree.Import _ | Tree.Export _
  | Tree.Alias _ ->
    ()
  | Tree.Error -> ()

(* ========================================
   PUBLIC API
   ======================================== *)

let check_program t program =
  List.iter (check_stmt t) program;
  !(t.diags)
