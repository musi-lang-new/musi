type symbol = int

type t = {
    mutable strings : string array
  ; mutable count : int
  ; table : (string, symbol) Hashtbl.t
}

let create () =
  { strings = Array.make 16 ""; count = 0; table = Hashtbl.create 256 }

let intern t str =
  match Hashtbl.find_opt t.table str with
  | Some sym -> sym
  | None ->
    let sym = t.count in
    if sym >= Array.length t.strings then (
      let new_cap = Array.length t.strings * 2 in
      let new_arr = Array.make new_cap "" in
      Array.blit t.strings 0 new_arr 0 (Array.length t.strings);
      t.strings <- new_arr);
    t.strings.(sym) <- str;
    t.count <- t.count + 1;
    Hashtbl.add t.table str sym;
    sym

let resolve t sym = t.strings.(sym)
let compare (a : symbol) (b : symbol) = Int.compare a b
let equals (a : symbol) (b : symbol) = a = b
let to_string t sym = resolve t sym
