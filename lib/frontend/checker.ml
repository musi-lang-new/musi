(** Infers and validates types using Hindley-Milner with gradual escape hatches. *)

(* ========================================
   TYPE REPRESENTATION
   ======================================== *)

type ty =
  | TyInt
  | TyNat
  | TyBool
  | TyText
  | TyUnit
  | TyProc of { params : ty list; ret : ty }
  | TyArray of ty
  | TyTuple of ty list
  | TyAny
  | TyNever
  | TyVar of ty_var ref
  | TyPtr of ty [@warning "-37"]
  (* TODO: use this *)
  | TyRef of ty [@warning "-37"]
(* TODO: use this *)

and ty_var = Unbound of int * int  (** id, level *) | Link of ty

(* ========================================
   TYPE CHECKER STATE
   ======================================== *)

type t = {
    interner : Interner.t
  ; resolver : Resolver.t [@warning "-69"] (* TODO: use this *)
  ; diags : Diagnostic.diagnostic_bag ref
  ; mutable next_var_id : int
  ; mutable curr_level : int [@warning "-69"] (* TODO: use this *)
}

let create interner resolver =
  {
    interner
  ; resolver
  ; diags = ref Diagnostic.empty_bag
  ; next_var_id = 0
  ; curr_level = 0
  }

let error t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg span)

(* ========================================
   TYPE UTILITIES
   ======================================== *)

let rec repr = function TyVar { contents = Link ty } -> repr ty | ty -> ty

let fresh_var t =
  let id = t.next_var_id in
  t.next_var_id <- t.next_var_id + 1;
  TyVar (ref (Unbound (id, t.curr_level)))

let rec occurs id level = function
  | TyVar ({ contents = Unbound (id', level') } as v) ->
    if id = id' then true
    else (
      if level' > level then v := Unbound (id', level);
      false)
  | TyVar { contents = Link ty } -> occurs id level ty
  | TyProc { params; ret } ->
    List.exists (occurs id level) params || occurs id level ret
  | TyArray ty | TyPtr ty | TyRef ty -> occurs id level ty
  | TyTuple tys -> List.exists (occurs id level) tys
  | TyInt | TyNat | TyBool | TyText | TyUnit | TyAny | TyNever -> false

let rec unify t ty1 ty2 span =
  let ty1 = repr ty1 in
  let ty2 = repr ty2 in
  match (ty1, ty2) with
  | TyVar ({ contents = Unbound (id, level) } as v), ty
  | ty, TyVar ({ contents = Unbound (id, level) } as v) ->
    if occurs id level ty then
      error t "cannot construct infinite type (occurs check)" span
    else v := Link ty
  | TyInt, TyInt
  | TyNat, TyNat
  | TyBool, TyBool
  | TyText, TyText
  | TyUnit, TyUnit ->
    ()
  | TyProc { params = p1; ret = r1 }, TyProc { params = p2; ret = r2 } ->
    if List.length p1 <> List.length p2 then
      error t "procedure arity mismatch" span
    else (
      List.iter2 (fun a b -> unify t a b span) p1 p2;
      unify t r1 r2 span)
  | TyArray t1, TyArray t2 -> unify t t1 t2 span
  | TyTuple ts1, TyTuple ts2 ->
    if List.length ts1 <> List.length ts2 then
      error t "tuple arity mismatch" span
    else List.iter2 (fun a b -> unify t a b span) ts1 ts2
  | TyAny, _ | _, TyAny -> ()
  | TyNever, _ | _, TyNever -> ()
  | _ -> error t "type mismatch" span

(* ========================================
   TYPE CHECKING
   ======================================== *)

let rec infer t (node : Node.node) : ty =
  match node.kind with
  | Node.ExprIntLit _ -> TyInt
  | Node.ExprBinLit _ -> TyNat
  | Node.ExprTextLit _ -> TyText
  | Node.ExprBoolLit _ -> TyBool
  | Node.ExprUnitLit -> TyUnit
  | Node.ExprIdent _ -> fresh_var t
  | Node.ExprBinary { op; lhs; rhs } -> (
    match op with
    | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.KwMod ->
      check t lhs TyInt;
      check t rhs TyInt;
      TyInt
    | Token.Eq | Token.EqSlashEq | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq
      ->
      let ty = infer t lhs in
      check t rhs ty;
      TyBool
    | Token.KwAnd | Token.KwOr | Token.KwXor ->
      check t lhs TyBool;
      check t rhs TyBool;
      TyBool
    | _ ->
      error t "unknown binary operator" node.span;
      TyNever)
  | Node.ExprUnary { op; operand } -> (
    match op with
    | Token.Minus ->
      check t operand TyInt;
      TyInt
    | Token.KwNot ->
      check t operand TyBool;
      TyBool
    | _ ->
      error t "unknown unary operator" node.span;
      TyNever)
  | Node.ExprCall { callee; args } ->
    let callee_ty = infer t callee in
    let ret_ty = fresh_var t in
    let param_tys = List.map (infer t) args.items in
    unify t callee_ty (TyProc { params = param_tys; ret = ret_ty }) node.span;
    ret_ty
  | Node.ExprIf { cond; then_br; else_br } -> (
    check t cond TyBool;
    let then_ty = infer t then_br in
    match else_br with
    | Some else_br ->
      let else_ty = infer t else_br in
      unify t then_ty else_ty node.span;
      then_ty
    | None ->
      unify t then_ty TyUnit node.span;
      TyUnit)
  | Node.ExprBlock { body; _ } ->
    if List.length body.items = 0 then TyUnit
    else
      let rec check_stmts = function
        | [] -> TyUnit
        | [ last ] -> infer t last
        | stmt :: rest ->
          let _ = infer t stmt in
          check_stmts rest
      in
      check_stmts body.items
  | Node.ExprArray { items } ->
    if List.length items.items = 0 then TyArray (fresh_var t)
    else
      let elem_ty = infer t (List.hd items.items) in
      List.iter (fun item -> check t item elem_ty) (List.tl items.items);
      TyArray elem_ty
  | Node.ExprTuple { items } -> TyTuple (List.map (infer t) items.items)
  | Node.ExprBinding { init; _ } -> infer t init
  | Node.ExprProc { params; ret_ty; body; unsafe_; external_; _ } ->
    if Option.is_some external_ && not unsafe_ then
      error t "external procedures require 'unsafe' modifier" node.span;
    let param_tys = List.map (fun _ -> fresh_var t) params.items in
    let ret = match ret_ty with Some _ -> fresh_var t | None -> fresh_var t in
    (match body with
    | Some body ->
      let body_ty = infer t body in
      unify t ret body_ty node.span
    | None -> ());
    TyProc { params = param_tys; ret }
  | Node.ExprCast { target; _ } -> (
    match target.kind with
    | Node.TyNamed { name } -> (
      let name_str = Interner.resolve t.interner name in
      match name_str with
      | "Int" -> TyInt
      | "Nat" -> TyNat
      | "Bool" -> TyBool
      | "Text" -> TyText
      | "Unit" -> TyUnit
      | "Any" -> TyAny
      | _ -> fresh_var t)
    | _ -> fresh_var t)
  | Node.ExprTest _ -> TyBool
  | Node.ExprReturn { value } -> (
    match value with
    | Some v ->
      let _ = infer t v in
      TyNever
    | None -> TyNever)
  | Node.ExprBreak _ -> TyNever
  | Node.ExprContinue -> TyNever
  | _ -> fresh_var t

and check t node expected =
  let actual = infer t node in
  unify t actual expected node.span

let check t program =
  List.iter
    (fun node ->
      let _ = infer t node in
      ())
    program;
  !(t.diags)
