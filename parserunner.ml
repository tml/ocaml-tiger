open Ast
open Printf

let string_of_pos pos =
  "(" ^ string_of_int (Lexer.line pos) ^ ", " ^ string_of_int (Lexer.col pos) ^ ")"

let rec string_of_exp = function
  | Int(x, pos)                 -> sprintf "Int(%d, %s)" x (string_of_pos pos)
  | String(s, pos)              -> sprintf "String(%s, %s)" s (string_of_pos pos)
  | LValue(l, pos)              -> sprintf "LValue(%s, %s)" (string_of_lvalue l) (string_of_pos pos)
  | Nil pos                     -> sprintf "Nil(%s)" (string_of_pos pos)
  | Break pos                   -> sprintf "Break(%s)" (string_of_pos pos)
  | FunCall(id, exps, pos)      -> sprintf "FunCall(%s, [%s], %s)" (Symbol.name id) (String.concat ", " (List.map string_of_exp exps)) (string_of_pos pos)
  | ArithExp(ae, pos)           -> sprintf "ArithExp(%s, %s)" (string_of_arith_exp ae) (string_of_pos pos)
  | CmpExp(ce, pos)             -> sprintf "CmpExp(%s, %s)" (string_of_cmp_exp ce) (string_of_pos pos)
  | BoolExp(be, pos)            -> sprintf "BoolExp(%s, %s)" (string_of_bool_exp be) (string_of_pos pos)
  | ExpSeq(es, pos)             -> sprintf "ExpSeq([%s], %s)" (String.concat "; " (List.map string_of_exp es)) (string_of_pos pos)
  | Assign(lv, e, pos)          -> sprintf "Assign(%s := %s, %s)" (string_of_lvalue lv) (string_of_exp e) (string_of_pos pos)
  | IfThen(e1, e2, pos)         -> sprintf "IfThen(%s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_pos pos)
  | IfThenElse(e1, e2, e3, pos) ->
    sprintf "IfThenElse(%s, %s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3) (string_of_pos pos)
  | While(e1, e2, pos)          -> sprintf "While(%s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_pos pos)
  | For(id, e1, e2, e3, pos)    ->
    sprintf "For(%s, %s, %s, %s, %s)" (Symbol.name id) (string_of_exp e1) (string_of_exp e2) (string_of_exp e3) (string_of_pos pos)
  | LetExp(ds, es, pos) ->
    sprintf "Let(%s, %s, %s)"
      (String.concat "\n" (List.map string_of_decl ds))
      (String.concat "\n" (List.map string_of_exp es))
      (string_of_pos pos)
  | Array(typ, size, init, pos) -> sprintf "Array(%s, %s, %s, %s)" (Symbol.name typ) (string_of_exp size) (string_of_exp init) (string_of_pos pos)
  | Record(typ, fields, pos)    ->
    sprintf "Record(%s, { %s }, %s)"
      (Symbol.name typ)
      (String.concat "; "
         (List.map (fun (field, value) -> Symbol.name field ^ "=" ^ string_of_exp value) fields))
      (string_of_pos pos)

and string_of_lvalue = function
  | Ident(v, pos)            -> sprintf "Ident(%s, %s)" (Symbol.name v) (string_of_pos pos)
  | ArrayAccess(l, e, pos)   -> sprintf "ArrayAccess(%s[%s] %s)" (string_of_lvalue l) (string_of_exp e) (string_of_pos pos)
  | RecordAccess(l, id, pos) -> sprintf "RecordAccess(%s.%s, %s)" (string_of_lvalue l) (Symbol.name id) (string_of_pos pos)

and string_of_arith_exp = function
  | Add(x, y) -> string_of_binop "+" x y
  | Sub(x, y) -> string_of_binop "-" x y
  | Mul(x, y) -> string_of_binop "*" x y
  | Div(x, y) -> string_of_binop "/" x y

and string_of_cmp_exp = function
  | Eq(x, y)  -> string_of_binop "="  x y
  | Neq(x, y) -> string_of_binop "<>" x y
  | Lt(x, y)  -> string_of_binop "<"  x y
  | Le(x, y)  -> string_of_binop "<=" x y
  | Gt(x, y)  -> string_of_binop ">"  x y
  | Ge(x, y)  -> string_of_binop ">=" x y

and string_of_bool_exp = function
  | And(x, y) -> string_of_binop "&" x y
  | Or(x, y)  -> string_of_binop "|" x y

and string_of_tyfields sep tyfields =
  String.concat sep (List.map (fun (v,t) -> Symbol.name v ^ ": " ^ Symbol.name t) tyfields)

and string_of_type_spec = function
  | TypeId(i, pos)            -> sprintf "TypeId(%s, %s)" (Symbol.name i) (string_of_pos pos)
  | TypeArray(i, pos)         -> sprintf "TypeArray(%s, %s)" (Symbol.name i) (string_of_pos pos)
  | TypeRecord(tyfields, pos) -> sprintf "TypeRecord(%s, %s)" ("{" ^ string_of_tyfields "; " tyfields ^ "}") (string_of_pos pos)

and string_of_decl = function
  | VarDecl (i, None, e, pos)   -> sprintf "VarDecl(%s := %s, %s)" (Symbol.name i) (string_of_exp e) (string_of_pos pos)
  | VarDecl (i, Some t, e, pos) -> sprintf "VarDecl(%s: %s := %s, %s)" (Symbol.name i) (Symbol.name t) (string_of_exp e) (string_of_pos pos)
  | TypeDecl(ty_decls, pos)     -> sprintf "TypeDecl([%s], %s)" (String.concat "; " (List.map string_of_ty_decl ty_decls)) (string_of_pos pos)
  | FunDecl(fun_decls, pos)     -> sprintf "FunDecl([%s], %s)" (String.concat "; " (List.map string_of_fun_decl fun_decls)) (string_of_pos pos)

and string_of_binop op x y =
  string_of_exp x ^ op ^ string_of_exp y

and string_of_ty_decl (type_id, type_spec) =
  sprintf "Type(%s = %s)" (Symbol.name type_id) (string_of_type_spec type_spec)

and string_of_fun_decl (fun_id, params, return, body) =
  sprintf "Function(%s [%s]%s, %s)"
    (Symbol.name fun_id)
    (string_of_tyfields ", " params)
    (match return with
    | None -> ""
    | Some t -> ": " ^ Symbol.name t)
    (string_of_exp body)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Lexer.lexer lexbuf in
  print_endline (string_of_exp ast)
