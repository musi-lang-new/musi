(** File identifier *)
type file_id = int

(** Source location span *)
type t

(** Create span from file ID, start and end positions *)
val make : file_id -> int -> int -> t

(** Dummy span for testing *)
val dummy : t

(** Get file ID *)
val file : t -> file_id

(** Get start position *)
val start : t -> int

(** Get end position *)
val end_ : t -> int

(** Get span length *)
val len : t -> int
