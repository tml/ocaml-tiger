type pos = Lexing.position
type id = Symbol.symbol
type type_id = Symbol.symbol

type tyfields = (id * type_id) list
type type_spec =
  | TypeId of type_id * pos
  | TypeArray of type_id * pos
  | TypeRecord of tyfields * pos

type exp =
  | Int of int * pos
  | String of string * pos
  | LValue of lvalue * pos
  | Nil of pos
  | Break of pos
  | FunCall of id * exp list * pos
  | Array of type_id * exp * exp * pos
  | Record of type_id * (id * exp) list * pos
  | ExpSeq of exp list * pos
  | LetExp of decl list * exp list * pos
  | Assign of lvalue * exp * pos

  | ArithExp of arith_exp * pos
  | BoolExp of bool_exp * pos
  | CmpExp of cmp_exp * pos

  | While of exp * exp * pos
  | For of id * exp * exp * exp * pos
  | IfThen of exp * exp * pos
  | IfThenElse of exp * exp * exp * pos

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
  | VarDecl of id * type_id option * exp * pos
  | FunDecl of (id * tyfields * type_id option * exp) list * pos
  | TypeDecl of (type_id * type_spec) list * pos

and lvalue =
  | Ident of id * pos
  | ArrayAccess of lvalue * exp * pos
  | RecordAccess of lvalue * id * pos
