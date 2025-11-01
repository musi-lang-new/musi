(** Links modules by resolving imports and tracking exports. *)

(** Module metadata. *)
type module_info = {
    path : string
  ; ast : Node.program
  ; exports : Interner.symbol list
}

(** Linker state tracking loaded modules. *)
type t

(** Create linker with stdlib search paths. *)
val create : Interner.t -> string list -> t

(** Load module by import path, returns module info. *)
val load_module : t -> string -> (module_info, string) result

(** Get all loaded modules. *)
val all_modules : t -> module_info list
