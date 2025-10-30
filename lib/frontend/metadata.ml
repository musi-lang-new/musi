(** Metadata tables for GIL bytecode emission (Roslyn-style).

    Built from bound tree after semantic analysis. Used during emission for
    procedure calls, string literals, and type information. *)

(** Procedure metadata entry. *)
type proc_entry = {
    id : int
  ; name : Interner.symbol
  ; param_count : int
  ; ret_type : int
  ; mutable bytecode_offset : int
  ; extern_ : bool
  ; extern_abi : Node.abi option
}

(** String literal metadata entry. *)
type string_entry = { id : int; symbol : Interner.symbol; bytes : bytes }

(** Type metadata entry. *)
type type_entry = { id : int; name : string; size : int; alignment : int }

(** Metadata tables. *)
type t = {
    procs : proc_entry list ref
  ; strings : string_entry list ref
  ; types : type_entry list ref
  ; mutable next_proc_id : int
  ; mutable next_string_id : int
  ; mutable next_type_id : int
}

let create () =
  {
    procs = ref []
  ; strings = ref []
  ; types = ref []
  ; next_proc_id = 0
  ; next_string_id = 0
  ; next_type_id = 0
  }

let add_proc t name param_count ret_type extern_ extern_abi =
  let id = t.next_proc_id in
  t.next_proc_id <- t.next_proc_id + 1;
  let entry =
    {
      id
    ; name
    ; param_count
    ; ret_type
    ; bytecode_offset = -1
    ; extern_
    ; extern_abi
    }
  in
  t.procs := entry :: !(t.procs);
  id

let add_string t symbol =
  let id = t.next_string_id in
  t.next_string_id <- t.next_string_id + 1;
  let entry = { id; symbol; bytes = Bytes.empty } in
  t.strings := entry :: !(t.strings);
  id

let add_type t name size alignment =
  let id = t.next_type_id in
  t.next_type_id <- t.next_type_id + 1;
  let entry = { id; name; size; alignment } in
  t.types := entry :: !(t.types);
  id

let get_proc t id = List.find (fun (e : proc_entry) -> e.id = id) !(t.procs)

let get_string t id =
  List.find (fun (e : string_entry) -> e.id = id) !(t.strings)

let get_type t id = List.find (fun (e : type_entry) -> e.id = id) !(t.types)

let set_proc_offset t id offset =
  let entry = get_proc t id in
  entry.bytecode_offset <- offset

let all_procs t = List.rev !(t.procs)
let all_strings t = List.rev !(t.strings)
let all_types t = List.rev !(t.types)
