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
  type t = symbol
  let compare (_, x) (_, y) = compare x y
end)

type 'a table = 'a SymbolTable.t

let empty = SymbolTable.empty

let put symtable key value = SymbolTable.add key value symtable

let get symtable key =
  try
    Some (SymbolTable.find key symtable)
  with Not_found ->
    None
