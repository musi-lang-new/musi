type symbol_kind =
  | Bind of { mutable_ : bool; ty : Musi_syntax.Tree.ty option }
  | Proc of {
        params : Musi_syntax.Tree.param list
      ; ret_ty : Musi_syntax.Tree.ty option
    }
  | Type of { fields : Musi_syntax.Tree.field list option }

type symbol = {
    name : Musi_shared.Interner.symbol
  ; kind : symbol_kind
  ; span : Musi_shared.Span.t
}

type scope = {
    symbols : (Musi_shared.Interner.symbol, symbol) Hashtbl.t
  ; parent : scope option
}

type t = {
    curr_scope : scope ref
  ; diags : Musi_shared.Diagnostic.diagnostic_bag ref
  ; interner : Musi_shared.Interner.t
}

let make_scope parent = { symbols = Hashtbl.create 16; parent }

let create interner =
  {
    curr_scope = ref (make_scope None)
  ; diags = ref Musi_shared.Diagnostic.empty_bag
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
        "duplicate definition of '%s'"
        (Musi_shared.Interner.to_string t.interner sym.name)
    in
    t.diags :=
      Musi_shared.Diagnostic.add
        !(t.diags)
        (Musi_shared.Diagnostic.error msg sym.span);
    t.diags :=
      Musi_shared.Diagnostic.add
        !(t.diags)
        (Musi_shared.Diagnostic.note "previous definition here" existing.span)
  | None -> Hashtbl.add !(t.curr_scope).symbols sym.name sym
