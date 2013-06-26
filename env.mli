type ty
type access
type env_entry =
  | VarEntry of ty
  | FunEntry of ty list * ty

val base_env : env_entry Symbol.table
val base_tenv : ty Symbol.table
