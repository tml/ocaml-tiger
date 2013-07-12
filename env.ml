type ty = Types.t
type access = unit
type env_entry =
  | VarEntry of ty
  | FunEntry of ty list * ty

let (|>) x f = f x


let base_tenv =
  List.fold_left (fun symtable (sym, ty) -> Symbol.put symtable sym ty)
    Symbol.empty
    [Symbol.symbol "int", Types.Int;
     Symbol.symbol "string", Types.String]

let base_venv =
  List.fold_left (fun symtable (sym, ty) -> Symbol.put symtable sym ty)
    Symbol.empty
    [Symbol.symbol "print",     FunEntry([Types.String], Types.Unit);
     Symbol.symbol "flush",     FunEntry([], Types.Unit);
     Symbol.symbol "getchar",   FunEntry([], Types.String);
     Symbol.symbol "ord",       FunEntry([Types.String], Types.Int);
     Symbol.symbol "chr",       FunEntry([Types.Int], Types.String);
     Symbol.symbol "size",      FunEntry([Types.String], Types.Int);
     Symbol.symbol "substring", FunEntry([Types.String; Types.Int; Types.Int], Types.String);
     Symbol.symbol "concat",    FunEntry([Types.String; Types.String], Types.String);
     Symbol.symbol "not",       FunEntry([Types.Int], Types.Int);
     Symbol.symbol "exit",      FunEntry([Types.Int], Types.Unit);
    ]
