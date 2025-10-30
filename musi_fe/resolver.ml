(** Links identifiers to their declarations and detects undefined names. *)

type symbol = {
    name : Interner.symbol
  ; kind : symbol_kind
  ; span : Span.t
  ; mutable ty : int option
}

and symbol_kind =
  | SymVar of { mutable_ : bool; weak : bool }
  | SymProc of { params : int; extern_ : Node.abi option }

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

let rec resolve_node t (node : Node.node) =
  match node.kind with
  | Node.ExprBind { mutable_; weakness; pat; init; _ } ->
    resolve_node t init;
    resolve_pattern t pat mutable_ weakness
  | Node.ExprProc { params; body; _ } ->
    enter_scope t;
    List.iter (resolve_param t) params.items;
    Option.iter (resolve_node t) body;
    exit_scope t
  | Node.ExprIdent { name } -> (
    match lookup t name with
    | Some _sym -> node.sym <- Some 0
    | None ->
      error
        t
        (Printf.sprintf
           "undefined name: '%s'"
           (Interner.resolve t.interner name))
        node.span)
  | Node.ExprCall { callee; args } ->
    resolve_node t callee;
    List.iter (resolve_node t) args.items
  | Node.ExprBinary { lhs; rhs; _ } ->
    resolve_node t lhs;
    resolve_node t rhs
  | Node.ExprUnary { operand; _ } -> resolve_node t operand
  | Node.ExprAssign { lhs; rhs } ->
    resolve_node t lhs;
    resolve_node t rhs
  | Node.ExprField { receiver; _ } -> resolve_node t receiver
  | Node.ExprIndex { receiver; index } ->
    resolve_node t receiver;
    resolve_node t index
  | Node.ExprIf { cond; then_br; else_br } ->
    resolve_node t cond;
    resolve_node t then_br;
    Option.iter (resolve_node t) else_br
  | Node.ExprMatch { scrutinee; cases } ->
    resolve_node t scrutinee;
    List.iter (resolve_match_case t) cases.items
  | Node.ExprWhile { cond; body } ->
    resolve_node t cond;
    resolve_node t body
  | Node.ExprFor { pat; iterable; body } ->
    resolve_node t iterable;
    enter_scope t;
    resolve_pattern t pat false false;
    resolve_node t body;
    exit_scope t
  | Node.ExprBlock { body; _ } ->
    enter_scope t;
    List.iter (resolve_node t) body.items;
    exit_scope t
  | Node.ExprReturn { value } -> Option.iter (resolve_node t) value
  | Node.ExprBreak { value } -> Option.iter (resolve_node t) value
  | Node.ExprArray { items } -> List.iter (resolve_node t) items.items
  | Node.ExprArrayList { item; count } ->
    resolve_node t item;
    resolve_node t count
  | Node.ExprTuple { items } -> List.iter (resolve_node t) items.items
  | Node.ExprRecordLit { fields } ->
    List.iter
      (fun (f : Node.record_field) -> resolve_node t f.value)
      fields.items
  | Node.ExprRange { start; end_; _ } ->
    resolve_node t start;
    resolve_node t end_
  | Node.ExprCast { inner; _ } -> resolve_node t inner
  | Node.ExprTest { inner; _ } -> resolve_node t inner
  | Node.ExprTry { inner } -> resolve_node t inner
  | Node.ExprDefer { inner } -> resolve_node t inner
  | Node.ExprIntLit _ | Node.ExprBinLit _ | Node.ExprTextLit _
  | Node.ExprBoolLit _ | Node.ExprUnitLit | Node.ExprContinue
  | Node.ExprImport _ | Node.ExprExport _ | Node.ExprRecord _
  | Node.ExprChoice _ | Node.ExprInterface _ | Node.PatWildcard | Node.PatRest _
  | Node.Error ->
    ()
  | Node.PatBind { inner } -> resolve_node t inner
  | Node.PatOr { alts } -> List.iter (resolve_node t) alts.items
  | Node.PatExpr { inner } -> resolve_node t inner

and resolve_pattern t pat mutable_ weak =
  match pat.kind with
  | Node.ExprIdent { name } ->
    let sym =
      { name; kind = SymVar { mutable_; weak }; span = pat.span; ty = None }
    in
    define t sym
  | Node.PatBind { inner } -> resolve_pattern t inner mutable_ weak
  | Node.ExprTuple { items } ->
    List.iter (fun p -> resolve_pattern t p mutable_ weak) items.items
  | Node.PatWildcard | Node.PatRest _ -> ()
  | _ -> resolve_node t pat

and resolve_param t (param : Node.param) =
  let sym =
    {
      name = param.name
    ; kind = SymVar { mutable_ = false; weak = false }
    ; span = param.span
    ; ty = None
    }
  in
  define t sym

and resolve_match_case t (case : Node.match_case) =
  enter_scope t;
  resolve_pattern t case.pat false false;
  Option.iter (resolve_node t) case.guard;
  resolve_node t case.body;
  exit_scope t

let resolve t program =
  List.iter (resolve_node t) program;
  !(t.diags)
