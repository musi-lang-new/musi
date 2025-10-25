(** Source file collection *)
type t

(** Individual source file *)
type file

(** File content digest *)
type digest = string

(** Empty source collection *)
val empty : t

(** Add file returning ID and updated collection *)
val add_file : t -> string -> string -> Span.file_id * t

(** Get file by ID *)
val get_file : t -> Span.file_id -> file option

(** Convert offset to line and column *)
val line_col : file -> int -> int * int

(** Get text of specific line *)
val line_text : file -> int -> string option

(** Get file path *)
val path : file -> string

(** Compute content digest *)
val compute_digest : string -> digest
