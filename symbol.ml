type symbol = string * int

let next_symbol = ref 0
let symbol_store : (string, int) Hashtbl.t = Hashtbl.create 64

let symbol name =
  try
    let index = Hashtbl.find symbol_store name in
    (name, index)
  with Not_found ->
    let index = !next_symbol in
    incr next_symbol;
    Hashtbl.add symbol_store name index;
    (name, index)

let name (s, _) = s


module SymbolTable = Map.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

type 'a table = 'a SymbolTable.t

let empty = SymbolTable.empty

let put symtable (_, index) value = SymbolTable.add index value symtable

let get symtable (_, index) =
  try
    Some (SymbolTable.find index symtable)
  with Not_found ->
    None
