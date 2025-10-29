(** Source location spans. *)

(** File identifier. *)
type file_id = int

(** Source location span. *)
type t = { file : file_id; start : int; end_ : int }

(** Create span from file ID, start and end positions. *)
let make file start end_ = { file; start; end_ }

(** Dummy span for testing. *)
let dummy = { file = 0; start = 0; end_ = 0 }

(** Get file ID. *)
let file t = t.file

(** Get start position. *)
let start t = t.start

(** Get end position. *)
let end_ t = t.end_

(** Get span length. *)
let len t = t.end_ - t.start
