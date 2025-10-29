(** Diagnostic severity levels *)
type severity = Error | Warning | Note

(** Individual diagnostic *)
type t = {
    severity : severity
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

(** Collection of diagnostics *)
type diagnostic_bag = { diags : t list; errors : int; warnings : int }

(** Create diagnostic *)
val make : severity -> string -> Span.t -> t

(** Create error diagnostic *)
val error : string -> Span.t -> t

(** Create warning diagnostic *)
val warning : string -> Span.t -> t

(** Create note diagnostic *)
val note : string -> Span.t -> t

(** Add note to diagnostic *)
val with_note : t -> string -> Span.t -> t

(** Empty diagnostic bag *)
val empty_bag : diagnostic_bag

(** Check if bag is empty *)
val is_empty : diagnostic_bag -> bool

(** Check if bag has errors *)
val has_errors : diagnostic_bag -> bool

(** Add diagnostic to bag *)
val add : diagnostic_bag -> t -> diagnostic_bag

(** Convert bag to list *)
val to_list : diagnostic_bag -> t list

(** Merge multiple bags *)
val merge : diagnostic_bag list -> diagnostic_bag

(** Emit single diagnostic *)
val emit : Format.formatter -> t -> Source.t -> unit

(** Emit all diagnostics in bag *)
val emit_all : Format.formatter -> diagnostic_bag -> Source.t -> unit
