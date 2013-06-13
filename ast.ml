type id = string
type type_id = string
type tyfields = (id * type_id) list

type exp =
  | Int of int
  | String of string
  | LValue of lvalue
  | Nil
  | FunCall of exp * exp list
  | ArrayCreation of exp * exp
  | RecordCreation of (id * exp) list
  | ExpSeq of exp list
  | LetExp of decl list * exp list
  | Assign of lvalue * exp

  | ArithExp of arith_exp
  | BoolExp of bool_exp
  | CmpExp of cmp_exp

and arith_exp =
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp

and bool_exp =
  | Or of exp * exp
  | And of exp * exp

and cmp_exp =
  | Eq of exp * exp
  | Neq of exp * exp
  | Lt of exp * exp
  | Le of exp * exp
  | Gt of exp * exp
  | Ge of exp * exp

and decl =
  | VarDecl of id * type_id option * exp
  | FuncDecl of id * tyfields * type_id option * exp

and lvalue =
  | Var of id
  | ArrayAccess of lvalue * id
  | RecordAccess of lvalue * exp
