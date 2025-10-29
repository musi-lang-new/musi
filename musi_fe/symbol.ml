type symbol_kind =
  | Bind of { mutable_ : bool; ty : Tree.ty option }
  | Proc of { params : Tree.param list; ret_ty : Tree.ty option }
  | Type of { fields : Tree.field list option }
  | Extern of { proc_id : int; lib_name : string }

type symbol = { name : Interner.symbol; kind : symbol_kind; span : Span.t }

type scope = {
    symbols : (Interner.symbol, symbol) Hashtbl.t
  ; parent : scope option
}

type t = {
    curr_scope : scope ref
  ; diags : Diagnostic.diagnostic_bag ref
  ; interner : Interner.t
}

let make_scope parent = { symbols = Hashtbl.create 16; parent }

let create interner =
  {
    curr_scope = ref (make_scope None)
  ; diags = ref Diagnostic.empty_bag
  ; interner
  }

let enter_scope t =
  let new_scope = make_scope (Some !(t.curr_scope)) in
  t.curr_scope := new_scope

let exit_scope t =
  match !(t.curr_scope).parent with
  | Some parent -> t.curr_scope := parent
  | None -> failwith "cannot exit root scope"

let rec lookup_in_scope scope name =
  match Hashtbl.find_opt scope.symbols name with
  | Some sym -> Some sym
  | None -> (
    match scope.parent with
    | Some parent -> lookup_in_scope parent name
    | None -> None)

let lookup t name = lookup_in_scope !(t.curr_scope) name

let define t sym =
  match Hashtbl.find_opt !(t.curr_scope).symbols sym.name with
  | Some existing ->
    let msg =
      Printf.sprintf
        "redefinition of '%s'"
        (Interner.to_string t.interner sym.name)
    in
    t.diags := Diagnostic.add !(t.diags) (Diagnostic.error msg sym.span);
    t.diags :=
      Diagnostic.add
        !(t.diags)
        (Diagnostic.note "previous definition here" existing.span)
  | None -> Hashtbl.add !(t.curr_scope).symbols sym.name sym

let rec iter_scope_symbols f scope =
  Hashtbl.iter (fun _ sym -> f sym) scope.symbols;
  match scope.parent with Some parent -> iter_scope_symbols f parent | None -> ()

let iter_all t f = iter_scope_symbols f !(t.curr_scope)
