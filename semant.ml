let undef = Translate.Undef

type venv = Env.env_entry Symbol.table
type tenv = Env.ty Symbol.table
type expty = {exp: Translate.exp; ty: Types.t}


(* val check_int : expty -> unit *)
let check_int {exp; ty} =
  if ty <> Types.Int then
    failwith "not an int"
  else
    ()

let rec translate_exp venv tenv exp =
  match exp with
  | Ast.Int x -> {exp=undef; ty=Types.Int}
  | Ast.String s -> {exp=undef; ty=Types.String}
  | Ast.Nil -> {exp=undef; ty=Types.Nil}
  | Ast.Break -> {exp=undef; ty=Types.Unit}
  | Ast.ArithExp ae -> translate_arith_exp venv tenv ae
  | Ast.BoolExp be -> translate_bool_exp venv tenv be
  | Ast.CmpExp ce -> translate_cmp_exp venv tenv ce

and translate_arith_exp venv tenv ae =
  match ae with
  | Ast.Add(left, right) -> (
    check_int (translate_exp venv tenv left);
    check_int (translate_exp venv tenv right);
    {exp=undef; ty=Types.Int}
  )
  | Ast.Sub(left, right) -> (
    check_int (translate_exp venv tenv left);
    check_int (translate_exp venv tenv right);
    {exp=undef; ty=Types.Int}
  )
  | Ast.Mul(left, right) -> (
    check_int (translate_exp venv tenv left);
    check_int (translate_exp venv tenv right);
    {exp=undef; ty=Types.Int}
  )
  | Ast.Div(left, right) -> (
    check_int (translate_exp venv tenv left);
    check_int (translate_exp venv tenv right);
    {exp=undef; ty=Types.Int}
  )

and translate_bool_exp venv tenv be =
  match be with
  | Ast.And(left, right) -> (
    check_int (translate_exp venv tenv left);
    check_int (translate_exp venv tenv right);
    {exp=undef; ty=Types.Int}
  )
  | Ast.Or(left, right) -> (
    check_int (translate_exp venv tenv left);
    check_int (translate_exp venv tenv right);
    {exp=undef; ty=Types.Int}
  )


and translate_cmp_exp venv tenv ce =
  failwith "not implemented"

and translate_lvalue venv tenv lv =
  failwith "not implemented"

and translate_decl venv tenv decls =
  failwith "not implemented"

and translate_type tenv types =
  failwith "not implemented"
