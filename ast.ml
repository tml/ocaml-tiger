type id = string
type type_id = string
type tyfields = (id * type_id) list
type type_spec =
  | TypeId of type_id
  | TypeArray of type_id
  | TypeRecord of tyfields

type exp =
  | Int of int
  | String of string
  | LValue of lvalue
  | Nil
  | Break
  | FunCall of id * exp list
  | Array of type_id * exp * exp
  | Record of type_id * (id * exp) list
  | ExpSeq of exp list
  | LetExp of decl list * exp list
  | Assign of lvalue * exp

  | ArithExp of arith_exp
  | BoolExp of bool_exp
  | CmpExp of cmp_exp

  | While of exp * exp
  | For of id * exp * exp * exp
  | IfThen of exp * exp
  | IfThenElse of exp * exp * exp

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
  | FunDecl of (id * tyfields * type_id option * exp) list
  | TypeDecl of (type_id * type_spec) list

and lvalue =
  | Ident of id
  | ArrayAccess of lvalue * exp
  | RecordAccess of lvalue * id
