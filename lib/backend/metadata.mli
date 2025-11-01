(** Metadata tables for GIL bytecode emission.

    Roslyn-style metadata built from bound tree after semantic analysis.
    Provides procedure, string, and type information for bytecode generation. *)

(** Procedure metadata with signature and location information. *)
type proc_entry = {
    id : int
  ; name : Interner.symbol
  ; param_count : int
  ; ret_type : int
  ; mutable bytecode_offset : int
  ; extern_ : bool
  ; extern_abi : Node.abi option
}

(** String literal metadata with interned symbol and UTF-8 bytes. *)
type string_entry = { id : int; symbol : Interner.symbol; bytes : bytes }

(** Type metadata with layout information. *)
type type_entry = { id : int; name : string; size : int; alignment : int }

(** Metadata table collection. *)
type t

(** Create empty metadata tables. *)
val create : unit -> t

(** Register procedure and return assigned identifier. *)
val add_proc :
  t -> Interner.symbol -> int -> int -> bool -> Node.abi option -> int

(** Register string literal and return assigned identifier. *)
val add_string : t -> Interner.symbol -> int

(** Register type with layout and return assigned identifier. *)
val add_type : t -> string -> int -> int -> int

(** Look up procedure metadata by identifier. *)
val get_proc : t -> int -> proc_entry

(** Look up string metadata by identifier. *)
val get_string : t -> int -> string_entry

(** Look up type metadata by identifier. *)
val get_type : t -> int -> type_entry

(** Update bytecode offset for procedure after emission. *)
val set_proc_offset : t -> int -> int -> unit

(** Get all registered procedures in insertion order. *)
val all_procs : t -> proc_entry list

(** Get all registered strings in insertion order. *)
val all_strings : t -> string_entry list

(** Get all registered types in insertion order. *)
val all_types : t -> type_entry list
