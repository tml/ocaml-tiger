type unique = unit ref

type t =
  | Int
  | String
  | Nil
  | Unit
  | TypeId of Symbol.symbol * t option ref
  | TypeArray of t * unique
  | TypeRecord of (Symbol.symbol t) list * unique
