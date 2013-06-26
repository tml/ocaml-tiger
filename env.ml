type ty = Types.t
type access = unit
type env_entry =
  | VarEntry of Types.t
  | FunEntry of Types.t list * Types.t

let (|>) x f = f x


let base_tenv =
  Symbol.empty
  |> Symbol.put (Symbol.symbol "int") Types.Int
  |> Symbol.put (Symbol.symbol "string") Types.Strin

let base_env =
  Symbol.empty
  |> Symbol.put (Symbol.symbol "print") (FunEntry [Types.String], Types.Unit)
  |> Symbol.put (Symbol.symbol "flush") (FunEntry [], Types.Unit)
  |> Symbol.put (Symbol.symbol "getchar") (FunEntry [], Types.String)
  |> Symbol.put (Symbol.symbol "ord") (FunEntry [Types.String], Types.Int)
  |> Symbol.put (Symbol.symbol "chr") (FunEntry [Types.Int], Types.String)
  |> Symbol.put (Symbol.symbol "size") (FunEntry [Types.String], Types.Int)
  |> Symbol.put (Symbol.symbol "substring") (FunEntry [Types.String; Types.Int; Types.Int], Types.String)
  |> Symbol.put (Symbol.symbol "concat") (FunEntry [Types.String; Types.String], Types.String)
  |> Symbol.put (Symbol.symbol "not") (FunEntry [Types.Int], Types.Int)
  |> Symbol.put (Symbol.symbol "exit") (FunEntry [Types.Int], Types.Unit)
