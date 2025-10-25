type severity = Error | Warning | Note

type t = {
    severity : severity
  ; message : string
  ; span : Span.t
  ; notes : (string * Span.t) list
}

type diagnostic_bag = { diags : t list; errors : int; warnings : int }

val make : severity -> string -> Span.t -> t
val error : string -> Span.t -> t
val warning : string -> Span.t -> t
val note : string -> Span.t -> t
val with_note : t -> string -> Span.t -> t
val empty_bag : diagnostic_bag
val is_empty : diagnostic_bag -> bool
val has_errors : diagnostic_bag -> bool
val add : diagnostic_bag -> t -> diagnostic_bag
val to_list : diagnostic_bag -> t list
val merge : diagnostic_bag list -> diagnostic_bag
val emit : Format.formatter -> t -> Source.t -> unit
val emit_all : Format.formatter -> diagnostic_bag -> Source.t -> unit
