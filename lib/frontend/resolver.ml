(** Links identifiers to their declarations and detects undefined names. *)

type symbol = {
    name : Interner.symbol
  ; kind : symbol_kind
  ; span : Span.t
  ; mutable ty : int option
  ; module_path : string option
}

and symbol_kind =
  | SymVar of { mutable_ : bool; weak : bool }
  | SymProc of { params : int; extern_ : Node.abi option }
  | SymAlias of { target : Interner.symbol; target_module : string }

type scope = {
    symbols : (Interner.symbol, symbol) Hashtbl.t
  ; parent : scope option
}

type t = {
    interner : Interner.t
  ; linker : Linker.t option
  ; mutable curr_scope : scope
  ; diags : Diagnostic.diagnostic_bag ref
}

let make_scope parent = { symbols = Hashtbl.create 16; parent }

let create interner =
  {
    interner
  ; linker = None
  ; curr_scope = make_scope None
  ; diags = ref Diagnostic.empty_bag
  }

let create_with_linker interner linker =
  {
    interner
  ; linker = Some linker
  ; curr_scope = make_scope None
  ; diags = ref Diagnostic.empty_bag
  }

(* https://www.geeksforgeeks.org/dsa/introduction-to-levenshtein-distance/ *)
let levenshtein s1 s2 =
  let m = String.length s1 in
  let n = String.length s2 in
  let d = Array.make_matrix (m + 1) (n + 1) 0 in
  for i = 0 to m do
    d.(i).(0) <- i
  done;
  for j = 0 to n do
    d.(0).(j) <- j
  done;
  for j = 1 to n do
    for i = 1 to m do
      let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1 in
      d.(i).(j) <-
        min
          (d.(i - 1).(j) + 1)
          (min (d.(i).(j - 1) + 1) (d.(i - 1).(j - 1) + cost))
    done
  done;
  d.(m).(n)

let find_similar_names t name =
  let name_str = Interner.resolve t.interner name in
  let candidates = ref [] in
  let rec collect_from_scope scope =
    Hashtbl.iter
      (fun sym_name _ ->
        let sym_str = Interner.resolve t.interner sym_name in
        let dist = levenshtein name_str sym_str in
        if dist <= 2 && dist > 0 then
          candidates := (sym_str, dist) :: !candidates)
      scope.symbols;
    match scope.parent with Some p -> collect_from_scope p | None -> ()
  in
  collect_from_scope t.curr_scope;
  List.sort (fun (_, d1) (_, d2) -> compare d1 d2) !candidates

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

let resolve_expr_ident t (node : Node.node) name =
  match lookup t name with
  | Some _sym -> node.sym <- Some 0
  | None ->
    let name_str = Interner.resolve t.interner name in
    let diag =
      Diagnostic.error
        (Printf.sprintf "use of undefined name '%s'" name_str)
        node.span
    in
    let diag =
      match find_similar_names t name with
      | (suggestion, _) :: _ ->
        Diagnostic.with_fixit
          diag
          { Diagnostic.span = node.span; replacement = suggestion }
      | [] -> diag
    in
    t.diags := Diagnostic.add !(t.diags) diag

let resolve_expr_import t (node : Node.node) source kind =
  match t.linker with
  | Some linker -> (
    let source_str = Interner.resolve t.interner source in
    match Linker.load_module linker source_str with
    | Ok module_info -> (
      match kind with
      | Node.Named { items } ->
        List.iter
          (fun (item : Node.import_export_item) ->
            if List.mem item.name module_info.exports then
              let import_name =
                match item.alias with Some a -> a | None -> item.name
              in
              let sym =
                {
                  name = import_name
                ; kind =
                    SymAlias { target = item.name; target_module = source_str }
                ; span = node.span
                ; ty = None
                ; module_path = Some source_str
                }
              in
              define t sym
            else
              error
                t
                (Printf.sprintf
                   "'%s' not exported by module '%s'"
                   (Interner.resolve t.interner item.name)
                   source_str)
                node.span)
          items
      | Node.Namespace _ -> ())
    | Error msg -> error t msg node.span)
  | None -> ()

let rec resolve_node t (node : Node.node) =
  match node.kind with
  | Node.ExprBinding { mutable_; weakness; pat; init; _ } ->
    resolve_node t init;
    resolve_pattern_with_init t pat mutable_ weakness init
  | Node.ExprProc { params; body; _ } ->
    enter_scope t;
    List.iter (resolve_param t) params.items;
    Option.iter (resolve_node t) body;
    exit_scope t
  | Node.ExprIdent { name } -> resolve_expr_ident t node name
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
  | Node.ExprImport { source; kind } -> resolve_expr_import t node source kind
  | Node.ExprIntLit _ | Node.ExprBinLit _ | Node.ExprTextLit _
  | Node.ExprBoolLit _ | Node.ExprUnitLit | Node.ExprContinue
  | Node.ExprExport _ | Node.ExprRecord _ | Node.ExprChoice _
  | Node.ExprInterface _ | Node.PatWildcard | Node.PatRest _ | Node.Error ->
    ()
  | Node.PatBind { inner } -> resolve_node t inner
  | Node.PatOr { alts } -> List.iter (resolve_node t) alts.items
  | Node.PatExpr { inner } -> resolve_node t inner

and resolve_pattern t pat mutable_ weak =
  match pat.kind with
  | Node.ExprIdent { name } ->
    let sym =
      {
        name
      ; kind = SymVar { mutable_; weak }
      ; span = pat.span
      ; ty = None
      ; module_path = None
      }
    in
    define t sym
  | Node.PatBind { inner } -> resolve_pattern t inner mutable_ weak
  | Node.ExprTuple { items } ->
    List.iter (fun p -> resolve_pattern t p mutable_ weak) items.items
  | Node.PatWildcard | Node.PatRest _ -> ()
  | _ -> resolve_node t pat

and resolve_pattern_with_init t pat mutable_ weak init =
  match (pat.kind, init.kind) with
  | Node.ExprIdent { name }, Node.ExprProc { params; external_; _ } ->
    let sym =
      {
        name
      ; kind =
          SymProc { params = List.length params.items; extern_ = external_ }
      ; span = pat.span
      ; ty = None
      ; module_path = None
      }
    in
    define t sym
  | _ -> resolve_pattern t pat mutable_ weak

and resolve_param t (param : Node.param) =
  let sym =
    {
      name = param.name
    ; kind = SymVar { mutable_ = false; weak = false }
    ; span = param.span
    ; ty = None
    ; module_path = None
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
