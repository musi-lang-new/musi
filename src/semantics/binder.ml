(* ========================================
   TYPES
   ======================================== *)

type t = { syms : Symbol.t; interner : Musi_shared.Interner.t }

(* ========================================
   CONSTRUCTORS
   ======================================== *)

let create interner = { syms = Symbol.create interner; interner }

(* ========================================
   EXPRESSION BINDING
   ======================================== *)

let rec bind_expr t (expr : Musi_syntax.Tree.expr) =
  match expr.kind with
  | Musi_syntax.Tree.IntLit _ | Musi_syntax.Tree.BinLit _
  | Musi_syntax.Tree.TextLit _ | Musi_syntax.Tree.BoolLit _
  | Musi_syntax.Tree.UnitLit | Musi_syntax.Tree.Continue ->
    ()
  | Musi_syntax.Tree.Ident { name } -> bind_ident_expr t expr name
  | Musi_syntax.Tree.Binary { lhs; rhs; _ } ->
    bind_expr t lhs;
    bind_expr t rhs
  | Musi_syntax.Tree.Unary { operand; _ } -> bind_expr t operand
  | Musi_syntax.Tree.Call { callee; args } ->
    bind_expr t callee;
    List.iter (bind_expr t) args
  | Musi_syntax.Tree.If { cond; then_br; else_br } ->
    bind_expr t cond;
    bind_expr t then_br;
    Option.iter (bind_expr t) else_br
  | Musi_syntax.Tree.Match { expr; cases } -> bind_match_expr t expr cases
  | Musi_syntax.Tree.Block { stmts } -> bind_block_expr t stmts
  | Musi_syntax.Tree.Array { elems } | Musi_syntax.Tree.Tuple { elems } ->
    List.iter (bind_expr t) elems
  | Musi_syntax.Tree.ArrayRepeat { elem; count } ->
    bind_expr t elem;
    bind_expr t count
  | Musi_syntax.Tree.RecordLit { fields } ->
    List.iter (fun (_, e) -> bind_expr t e) fields
  | Musi_syntax.Tree.Field { receiver; _ } -> bind_expr t receiver
  | Musi_syntax.Tree.Index { receiver; index } ->
    bind_expr t receiver;
    bind_expr t index
  | Musi_syntax.Tree.Range { start; end_; _ } ->
    bind_expr t start;
    bind_expr t end_
  | Musi_syntax.Tree.Cast { expr; _ } | Musi_syntax.Tree.Test { expr; _ } ->
    bind_expr t expr
  | Musi_syntax.Tree.Bind { mutable_; pat; ty; init } ->
    bind_expr t init;
    bind_bind_expr t expr mutable_ pat ty
  | Musi_syntax.Tree.Assign { lhs; rhs } -> bind_assign_expr t expr lhs rhs
  | Musi_syntax.Tree.Return { value } | Musi_syntax.Tree.Break { value } ->
    Option.iter (bind_expr t) value
  | Musi_syntax.Tree.While { cond; body } -> bind_while_expr t cond body
  | Musi_syntax.Tree.For { pat; iter; body } -> bind_for_expr t pat iter body
  | Musi_syntax.Tree.Try { expr }
  | Musi_syntax.Tree.Defer { expr }
  | Musi_syntax.Tree.Async { expr }
  | Musi_syntax.Tree.Await { expr } ->
    bind_expr t expr
  | Musi_syntax.Tree.Proc { params; body; _ } -> bind_proc_expr t params body
  | Musi_syntax.Tree.Record _ | Musi_syntax.Tree.Choice _
  | Musi_syntax.Tree.Interface _ | Musi_syntax.Tree.Template _
  | Musi_syntax.Tree.Error ->
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
        (Musi_shared.Interner.to_string t.interner name)
    in
    t.syms.diags :=
      Musi_shared.Diagnostic.add
        !(t.syms.diags)
        (Musi_shared.Diagnostic.error msg expr.span)

and bind_bind_expr t expr mutable_ pat ty =
  match pat.Musi_syntax.Tree.kind with
  | Musi_syntax.Tree.Ident { name } ->
    let sym =
      { Symbol.name; kind = Symbol.Bind { mutable_; ty }; span = expr.span }
    in
    Symbol.define t.syms sym
  | _ -> bind_pat t pat

and bind_assign_expr t expr lhs rhs =
  bind_expr t lhs;
  bind_expr t rhs;
  match lhs.kind with
  | Musi_syntax.Tree.Ident { name } -> (
    match Symbol.lookup t.syms name with
    | Some { kind = Symbol.Bind { mutable_ = true; _ }; _ } -> ()
    | Some { kind = Symbol.Bind { mutable_ = false; _ }; span; _ } ->
      let msg =
        Printf.sprintf
          "cannot assign to immutable binding: '%s'"
          (Musi_shared.Interner.to_string t.interner name)
      in
      t.syms.diags :=
        Musi_shared.Diagnostic.add
          !(t.syms.diags)
          (Musi_shared.Diagnostic.error msg expr.span);
      t.syms.diags :=
        Musi_shared.Diagnostic.add
          !(t.syms.diags)
          (Musi_shared.Diagnostic.note "binding declared here" span)
    | Some _ ->
      let msg =
        Printf.sprintf
          "'%s' is not binding"
          (Musi_shared.Interner.to_string t.interner name)
      in
      t.syms.diags :=
        Musi_shared.Diagnostic.add
          !(t.syms.diags)
          (Musi_shared.Diagnostic.error msg expr.span)
    | None ->
      let msg =
        Printf.sprintf
          "undeclared identifier: '%s'"
          (Musi_shared.Interner.to_string t.interner name)
      in
      t.syms.diags :=
        Musi_shared.Diagnostic.add
          !(t.syms.diags)
          (Musi_shared.Diagnostic.error msg expr.span))
  | _ -> ()

and bind_match_case t (case : Musi_syntax.Tree.match_case) =
  bind_pat t case.pat;
  Option.iter (bind_expr t) case.guard;
  bind_expr t case.body

and bind_pat t (pat : Musi_syntax.Tree.pat) =
  match pat.kind with
  | Musi_syntax.Tree.Wildcard | Musi_syntax.Tree.IntLit _
  | Musi_syntax.Tree.BoolLit _ | Musi_syntax.Tree.TextLit _ ->
    ()
  | Musi_syntax.Tree.Ident { name } ->
    let sym =
      {
        Symbol.name
      ; kind = Symbol.Bind { mutable_ = false; ty = None }
      ; span = pat.span
      }
    in
    Symbol.define t.syms sym
  | Musi_syntax.Tree.Tuple { pats }
  | Musi_syntax.Tree.Array { pats }
  | Musi_syntax.Tree.Or { pats } ->
    List.iter (bind_pat t) pats
  | Musi_syntax.Tree.Record { fields } ->
    List.iter (fun (_, p) -> bind_pat t p) fields
  | Musi_syntax.Tree.Range { start; end_; _ } ->
    bind_pat t start;
    bind_pat t end_
  | Musi_syntax.Tree.Choice { pat = Some p; _ } -> bind_pat t p
  | Musi_syntax.Tree.Choice { pat = None; _ }
  | Musi_syntax.Tree.Rest _ | Musi_syntax.Tree.ValueBinding _
  | Musi_syntax.Tree.Error ->
    ()

and bind_param t (param : Musi_syntax.Tree.param) =
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

and bind_stmt t (stmt : Musi_syntax.Tree.stmt) =
  match stmt.kind with
  | Musi_syntax.Tree.Expr { expr } -> bind_expr t expr
  | Musi_syntax.Tree.Error -> ()

(* ========================================
   DECLARATION BINDING
   ======================================== *)

let bind_decl t (decl : Musi_syntax.Tree.decl) =
  match decl.kind with
  | Musi_syntax.Tree.Stmt { stmt } -> bind_stmt t stmt
  | Musi_syntax.Tree.Import _ | Musi_syntax.Tree.Export _
  | Musi_syntax.Tree.Alias _ ->
    ()
  | Musi_syntax.Tree.Error -> ()

(* ========================================
   PUBLIC API
   ======================================== *)

let bind_program t program =
  List.iter (bind_decl t) program;
  !(t.syms.diags)
