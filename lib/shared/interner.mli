(** Deduplicates strings by mapping them to integer IDs *)
type t

(** Unique integer representing an interned string *)
type symbol = private int

(** Allocates a new empty interner *)
val create : unit -> t

(** Returns the symbol for a string, creating it if needed *)
val intern : t -> string -> symbol

(** Retrieves the original string for a symbol *)
val resolve : t -> symbol -> string

(** Orders two symbols numerically *)
val compare : symbol -> symbol -> int

(** Tests whether two symbols represent the same string *)
val equals : symbol -> symbol -> bool

(** Retrieves the original string for a symbol *)
val to_string : t -> symbol -> string
