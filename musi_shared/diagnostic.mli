(** Classifies diagnostic messages by importance. *)
type severity = Error | Warning | Note

(** Represents a suggested code fix with location and replacement text. *)
type fixit = { span : Span.t; replacement : string }

(** Represents a compiler message with location and optional context notes. *)
type t = {
    severity : severity
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
  ; fixits : fixit list
}

(** Accumulates diagnostics with error and warning counts. *)
type diagnostic_bag = { diags : t list; errors : int; warnings : int }

(** Constructs a diagnostic from severity, message, and location. *)
val make : severity -> string -> Span.t -> t

(** Constructs an error-level diagnostic. *)
val error : string -> Span.t -> t

(** Constructs a warning-level diagnostic. *)
val warning : string -> Span.t -> t

(** Constructs a note-level diagnostic. *)
val note : string -> Span.t -> t

(** Attaches a contextual note to an existing diagnostic. *)
val with_note : t -> string -> Span.t -> t

(** Attaches a fix-it suggestion to an existing diagnostic. *)
val with_fixit : t -> fixit -> t

(** Returns a bag containing no diagnostics. *)
val empty_bag : diagnostic_bag

(** Tests whether a bag contains any diagnostics. *)
val is_empty : diagnostic_bag -> bool

(** Tests whether a bag contains any errors. *)
val has_errors : diagnostic_bag -> bool

(** Inserts a diagnostic into a bag, updating counts. *)
val add : diagnostic_bag -> t -> diagnostic_bag

(** Extracts diagnostics from a bag in emission order. *)
val to_list : diagnostic_bag -> t list

(** Combines multiple bags into one, preserving all diagnostics. *)
val merge : diagnostic_bag list -> diagnostic_bag

(** Formats and writes a diagnostic with source context. *)
val emit : Format.formatter -> t -> Source.t -> unit

(** Formats and writes all diagnostics in a bag. *)
val emit_all : Format.formatter -> diagnostic_bag -> Source.t -> unit
