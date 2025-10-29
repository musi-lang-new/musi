(* ========================================
   TYPES
   ======================================== *)

type t = { syms : Symbol.t; interner : Interner.t }

let create interner = { syms = Symbol.create interner; interner }

(* ========================================
   EXPRESSION BINDING
   ======================================== *)

let rec bind_expr t (expr : Tree.expr) =
  match expr.kind with
  | Tree.IntLit _ | Tree.BinLit _ | Tree.TextLit _ | Tree.BoolLit _
  | Tree.UnitLit | Tree.Continue ->
    ()
  | Tree.Ident { name } -> bind_ident_expr t expr name
  | Tree.Binary { lhs; rhs; _ } ->
    bind_expr t lhs;
    bind_expr t rhs
  | Tree.Unary { operand; _ } -> bind_expr t operand
  | Tree.Call { callee; args } ->
    bind_expr t callee;
    List.iter (bind_expr t) args
  | Tree.If { cond; then_br; else_br } ->
    bind_expr t cond;
    bind_expr t then_br;
    Option.iter (bind_expr t) else_br
  | Tree.Match { expr; cases } -> bind_match_expr t expr cases
  | Tree.Block { stmts } -> bind_block_expr t stmts
  | Tree.Array { elems } | Tree.Tuple { elems } -> List.iter (bind_expr t) elems
  | Tree.ArrayRepeat { elem; count } ->
    bind_expr t elem;
    bind_expr t count
  | Tree.RecordLit { fields } -> List.iter (fun (_, e) -> bind_expr t e) fields
  | Tree.Field { receiver; _ } -> bind_expr t receiver
  | Tree.Index { receiver; index } ->
    bind_expr t receiver;
    bind_expr t index
  | Tree.Range { start; end_; _ } ->
    bind_expr t start;
    bind_expr t end_
  | Tree.Cast { expr; _ } | Tree.Test { expr; _ } -> bind_expr t expr
  | Tree.Bind { mutable_; pat; ty; init } ->
    bind_expr t init;
    let modifiers = Tree.default_modifiers in
    bind_bind_expr t expr mutable_ pat ty init modifiers
  | Tree.Assign { lhs; rhs } -> bind_assign_expr t expr lhs rhs
  | Tree.Return { value } | Tree.Break { value } ->
    Option.iter (bind_expr t) value
  | Tree.While { cond; body } -> bind_while_expr t cond body
  | Tree.For { pat; iter; body } -> bind_for_expr t pat iter body
  | Tree.Try { expr }
  | Tree.Defer { expr }
  | Tree.Async { expr }
  | Tree.Await { expr } ->
    bind_expr t expr
  | Tree.Proc { params; body; _ } -> bind_proc_expr t params body
  | Tree.Record _ | Tree.Choice _ | Tree.Interface _ | Tree.Template _
  | Tree.Error ->
    ()

and bind_match_expr t expr cases =
  bind_expr t expr;
  List.iter (bind_match_case t) cases

and bind_block_expr t stmts =
  Symbol.enter_scope t.syms;
  List.iter (bind_stmt t) stmts;
  Symbol.exit_scope t.syms

and bind_while_expr t cond body =
  bind_expr t cond;
  Symbol.enter_scope t.syms;
  List.iter (bind_stmt t) body;
  Symbol.exit_scope t.syms

and bind_for_expr t pat iter body =
  bind_expr t iter;
  Symbol.enter_scope t.syms;
  bind_pat t pat;
  List.iter (bind_stmt t) body;
  Symbol.exit_scope t.syms

and bind_proc_expr t params body =
  Symbol.enter_scope t.syms;
  List.iter (bind_param t) params;
  Option.iter (fun stmts -> List.iter (bind_stmt t) stmts) body;
  Symbol.exit_scope t.syms

and bind_ident_expr t expr name =
  match Symbol.lookup t.syms name with
  | Some _ -> ()
  | None ->
    let msg =
      Printf.sprintf
        "undeclared identifier: '%s'"
        (Interner.to_string t.interner name)
    in
    t.syms.diags :=
      Diagnostic.add !(t.syms.diags) (Diagnostic.error msg expr.span)

and bind_bind_expr t expr mutable_ pat ty init modifiers =
  match pat.Tree.kind with
  | Tree.Ident { name } ->
    bind_bind_ident t expr mutable_ name ty init modifiers
  | _ -> bind_pat t pat

and is_builtin_name interner name =
  let str = Interner.to_string interner name in
  String.length str >= 10 && String.sub str 0 10 = "__builtin_"

and bind_bind_ident t bind_expr mutable_ name ty init modifiers =
  let is_extern_intrinsic =
    match modifiers.Tree.externness with
    | true, Some lib_name ->
      Interner.to_string t.interner lib_name = "intrinsic"
    | _ -> false
  in
  if is_builtin_name t.interner name && not is_extern_intrinsic then
    let msg = "'__builtin_*' name(s) reserved for compiler intrinsics" in
    t.syms.diags :=
      Diagnostic.add !(t.syms.diags) (Diagnostic.error msg bind_expr.span)
  else
    match init.kind with
    | Tree.Proc { body; _ } when is_extern_intrinsic && body = None ->
      let sym =
        {
          Symbol.name
        ; kind = Symbol.Extern { proc_id = 0; lib_name = "intrinsic" }
        ; span = bind_expr.span
        }
      in
      Symbol.define t.syms sym
    | Tree.Proc { params; ret_ty; _ } ->
      let sym =
        {
          Symbol.name
        ; kind = Symbol.Proc { params; ret_ty }
        ; span = bind_expr.span
        }
      in
      Symbol.define t.syms sym
    | _ ->
      let sym =
        {
          Symbol.name
        ; kind = Symbol.Bind { mutable_; ty }
        ; span = bind_expr.span
        }
      in
      Symbol.define t.syms sym

and bind_assign_expr t expr lhs rhs =
  bind_expr t lhs;
  bind_expr t rhs;
  match lhs.kind with
  | Tree.Ident { name } -> (
    match Symbol.lookup t.syms name with
    | Some { kind = Symbol.Bind { mutable_ = true; _ }; _ } -> ()
    | Some { kind = Symbol.Bind { mutable_ = false; _ }; span; _ } ->
      let msg =
        Printf.sprintf
          "cannot assign to immutable binding: '%s'"
          (Interner.to_string t.interner name)
      in
      t.syms.diags :=
        Diagnostic.add !(t.syms.diags) (Diagnostic.error msg expr.span);
      t.syms.diags :=
        Diagnostic.add
          !(t.syms.diags)
          (Diagnostic.note "binding declared here" span)
    | Some _ ->
      let msg =
        Printf.sprintf
          "'%s' is not binding"
          (Interner.to_string t.interner name)
      in
      t.syms.diags :=
        Diagnostic.add !(t.syms.diags) (Diagnostic.error msg expr.span)
    | None ->
      let msg =
        Printf.sprintf
          "undeclared identifier: '%s'"
          (Interner.to_string t.interner name)
      in
      t.syms.diags :=
        Diagnostic.add !(t.syms.diags) (Diagnostic.error msg expr.span))
  | _ -> ()

and bind_match_case t (case : Tree.match_case) =
  bind_pat t case.pat;
  Option.iter (bind_expr t) case.guard;
  bind_expr t case.body

and bind_pat t (pat : Tree.pat) =
  match pat.kind with
  | Tree.Wildcard | Tree.IntLit _ | Tree.BoolLit _ | Tree.TextLit _ -> ()
  | Tree.Ident { name } ->
    let sym =
      {
        Symbol.name
      ; kind = Symbol.Bind { mutable_ = false; ty = None }
      ; span = pat.span
      }
    in
    Symbol.define t.syms sym
  | Tree.Tuple { pats } | Tree.Array { pats } | Tree.Or { pats } ->
    List.iter (bind_pat t) pats
  | Tree.Record { fields } -> List.iter (fun (_, p) -> bind_pat t p) fields
  | Tree.Range { start; end_; _ } ->
    bind_pat t start;
    bind_pat t end_
  | Tree.Choice { pat = Some p; _ } -> bind_pat t p
  | Tree.Choice { pat = None; _ }
  | Tree.Rest _ | Tree.ValueBinding _ | Tree.Error ->
    ()

and bind_param t (param : Tree.param) =
  let sym =
    {
      Symbol.name = param.name
    ; kind = Symbol.Bind { mutable_ = false; ty = Some param.ty }
    ; span = param.span
    }
  in
  Symbol.define t.syms sym

(* ========================================
   STATEMENT BINDING
   ======================================== *)

and bind_stmt t (stmt : Tree.stmt) =
  match stmt.kind with
  | Tree.Expr { expr } -> (
    match expr.kind with
    | Tree.Bind { mutable_; pat; ty; init } ->
      bind_expr t init;
      bind_bind_expr t expr mutable_ pat ty init stmt.modifiers
    | _ -> bind_expr t expr)
  | Tree.Import _ | Tree.Export _ | Tree.Alias _ -> ()
  | Tree.Error -> ()

(* ========================================
   PUBLIC API
   ======================================== *)

let bind_program t program =
  List.iter (bind_stmt t) program;
  !(t.syms.diags)
