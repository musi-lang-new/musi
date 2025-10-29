type symbol = int
type t = { mutable strings : string array; table : (string, symbol) Hashtbl.t }

let create () = { strings = [||]; table = Hashtbl.create 256 }

let intern t str =
  match Hashtbl.find_opt t.table str with
  | Some sym -> sym
  | None ->
    let sym = Array.length t.strings in
    t.strings <- Array.append t.strings [| str |];
    Hashtbl.add t.table str sym;
    sym

let resolve t sym = t.strings.(sym)
let compare (a : symbol) (b : symbol) = Int.compare a b
let equals (a : symbol) (b : symbol) = a = b
let to_string t sym = resolve t sym
