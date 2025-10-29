(** String interner *)
type t

(** Interned string symbol *)
type symbol = private int

(** Create new interner *)
val create : unit -> t

(** Intern string returning symbol *)
val intern : t -> string -> symbol

(** Resolve symbol to string *)
val resolve : t -> symbol -> string

(** Compare symbols *)
val compare : symbol -> symbol -> int

(** Test symbol equality *)
val equals : symbol -> symbol -> bool

(** Convert symbol to string *)
val to_string : t -> symbol -> string
