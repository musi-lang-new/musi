type t
type symbol = private int

val create : unit -> t
val intern : t -> string -> symbol
val resolve : t -> symbol -> string
val compare : symbol -> symbol -> int
val equals : symbol -> symbol -> bool
val to_string : t -> symbol -> string
