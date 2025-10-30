(** Represents byte ranges in source files for error reporting. *)

(** Identifies which source file a span refers to. *)
type file_id = int

(** Marks a contiguous byte range in a source file. *)
type t = { file : file_id; start : int; end_ : int }

(** Constructs a span from file ID and byte offsets. *)
let make file start end_ = { file; start; end_ }

(** Provides a placeholder span for synthetic nodes. *)
let dummy = { file = 0; start = 0; end_ = 0 }

(** Extracts the file ID from a span. *)
let file t = t.file

(** Extracts the starting byte offset. *)
let start t = t.start

(** Extracts the ending byte offset. *)
let end_ t = t.end_

(** Computes the number of bytes covered by a span. *)
let len t = t.end_ - t.start
