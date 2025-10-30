(** Links identifiers to their declarations and detects undefined names. *)

type symbol = {
    name : Interner.symbol
  ; kind : symbol_kind
  ; span : Span.t
  ; mutable ty : int option
}

and symbol_kind =
  | SymVar of { mutable_ : bool; weak : bool }
  | SymProc of { params : int; extern_ : Tree.abi option }

type scope = {
    symbols : (Interner.symbol, symbol) Hashtbl.t
  ; parent : scope option
}

type t = {
    interner : Interner.t
  ; mutable curr_scope : scope
  ; diags : Diagnostic.diagnostic_bag ref
}

let make_scope parent = { symbols = Hashtbl.create 16; parent }

let create interner =
  { interner; curr_scope = make_scope None; diags = ref Diagnostic.empty_bag }

let error t msg span =
  t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg span)

let enter_scope t = t.curr_scope <- make_scope (Some t.curr_scope)

let exit_scope t =
  match t.curr_scope.parent with
  | Some parent -> t.curr_scope <- parent
  | None -> failwith "cannot exit root scope"

let rec lookup_in_scope scope name =
  match Hashtbl.find_opt scope.symbols name with
  | Some sym -> Some sym
  | None -> (
    match scope.parent with
    | Some parent -> lookup_in_scope parent name
    | None -> None)

let lookup t name = lookup_in_scope t.curr_scope name

let define t sym =
  match Hashtbl.find_opt t.curr_scope.symbols sym.name with
  | Some existing ->
    error
      t
      (Printf.sprintf
         "duplicate definition of '%s'"
         (Interner.resolve t.interner sym.name))
      sym.span;
    error t "previous definition here" existing.span
  | None -> Hashtbl.add t.curr_scope.symbols sym.name sym

let rec resolve_node t (node : Tree.node) =
  match node.kind with
  | Tree.ExprBind { mutable_; weakness; pat; init; _ } ->
    resolve_node t init;
    resolve_pattern t pat mutable_ weakness
  | Tree.ExprProc { params; body; _ } ->
    enter_scope t;
    List.iter (resolve_param t) params.items;
    Option.iter (resolve_node t) body;
    exit_scope t
  | Tree.ExprIdent { name } -> (
    match lookup t name with
    | Some _sym -> node.sym <- Some 0
    | None ->
      error
        t
        (Printf.sprintf
           "undefined name: '%s'"
           (Interner.resolve t.interner name))
        node.span)
  | Tree.ExprCall { callee; args } ->
    resolve_node t callee;
    List.iter (resolve_node t) args.items
  | Tree.ExprBinary { lhs; rhs; _ } ->
    resolve_node t lhs;
    resolve_node t rhs
  | Tree.ExprUnary { operand; _ } -> resolve_node t operand
  | Tree.ExprAssign { lhs; rhs } ->
    resolve_node t lhs;
    resolve_node t rhs
  | Tree.ExprField { receiver; _ } -> resolve_node t receiver
  | Tree.ExprIndex { receiver; index } ->
    resolve_node t receiver;
    resolve_node t index
  | Tree.ExprIf { cond; then_br; else_br } ->
    resolve_node t cond;
    resolve_node t then_br;
    Option.iter (resolve_node t) else_br
  | Tree.ExprMatch { scrutinee; cases } ->
    resolve_node t scrutinee;
    List.iter (resolve_match_case t) cases.items
  | Tree.ExprWhile { cond; body } ->
    resolve_node t cond;
    resolve_node t body
  | Tree.ExprFor { pat; iterable; body } ->
    resolve_node t iterable;
    enter_scope t;
    resolve_pattern t pat false false;
    resolve_node t body;
    exit_scope t
  | Tree.ExprBlock { body; _ } ->
    enter_scope t;
    List.iter (resolve_node t) body.items;
    exit_scope t
  | Tree.ExprReturn { value } -> Option.iter (resolve_node t) value
  | Tree.ExprBreak { value } -> Option.iter (resolve_node t) value
  | Tree.ExprArray { items } -> List.iter (resolve_node t) items.items
  | Tree.ExprArrayList { item; count } ->
    resolve_node t item;
    resolve_node t count
  | Tree.ExprTuple { items } -> List.iter (resolve_node t) items.items
  | Tree.ExprRecordLit { fields } ->
    List.iter
      (fun (f : Tree.record_field) -> resolve_node t f.value)
      fields.items
  | Tree.ExprRange { start; end_; _ } ->
    resolve_node t start;
    resolve_node t end_
  | Tree.ExprCast { inner; _ } -> resolve_node t inner
  | Tree.ExprTest { inner; _ } -> resolve_node t inner
  | Tree.ExprTry { inner } -> resolve_node t inner
  | Tree.ExprDefer { inner } -> resolve_node t inner
  | Tree.ExprIntLit _ | Tree.ExprBinLit _ | Tree.ExprTextLit _
  | Tree.ExprBoolLit _ | Tree.ExprUnitLit | Tree.ExprContinue
  | Tree.ExprImport _ | Tree.ExprExport _ | Tree.ExprRecord _
  | Tree.ExprChoice _ | Tree.ExprInterface _ | Tree.PatWildcard | Tree.PatRest _
  | Tree.Error ->
    ()
  | Tree.PatBind { inner } -> resolve_node t inner
  | Tree.PatOr { alts } -> List.iter (resolve_node t) alts.items
  | Tree.PatExpr { inner } -> resolve_node t inner

and resolve_pattern t pat mutable_ weak =
  match pat.kind with
  | Tree.ExprIdent { name } ->
    let sym =
      { name; kind = SymVar { mutable_; weak }; span = pat.span; ty = None }
    in
    define t sym
  | Tree.PatBind { inner } -> resolve_pattern t inner mutable_ weak
  | Tree.ExprTuple { items } ->
    List.iter (fun p -> resolve_pattern t p mutable_ weak) items.items
  | Tree.PatWildcard | Tree.PatRest _ -> ()
  | _ -> resolve_node t pat

and resolve_param t (param : Tree.param) =
  let sym =
    {
      name = param.name
    ; kind = SymVar { mutable_ = false; weak = false }
    ; span = param.span
    ; ty = None
    }
  in
  define t sym

and resolve_match_case t (case : Tree.match_case) =
  enter_scope t;
  resolve_pattern t case.pat false false;
  Option.iter (resolve_node t) case.guard;
  resolve_node t case.body;
  exit_scope t

let resolve t program =
  List.iter (resolve_node t) program;
  !(t.diags)
