open Ast

let rec string_of_exp = function
  | Int x -> string_of_int x
  | String s -> "\"" ^ s ^ "\""
  | LValue l -> string_of_lvalue l
  | Nil -> "nil"
  | Break -> "break"
  | FunCall(id, exps) -> Printf.sprintf "%s(%s)" id (String.concat ", " (List.map string_of_exp exps))
  | ArithExp ae -> string_of_arith_exp ae
  | CmpExp ce -> string_of_cmp_exp ce
  | BoolExp be -> string_of_bool_exp be
  | ExpSeq es -> "(" ^ (String.concat "; " (List.map string_of_exp es)) ^ ")"
  | Assign(lv, e) -> string_of_lvalue lv ^ " := " ^ string_of_exp e
  | IfThen(e1, e2) -> Printf.sprintf "if %s then %s end" (string_of_exp e1) (string_of_exp e2)
  | IfThenElse(e1, e2, e3) ->
    Printf.sprintf "if %s then %s else %s end" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | While(e1, e2) ->
    Printf.sprintf "while %s do %s done" (string_of_exp e1) (string_of_exp e2)
  | For(id, e1, e2, e3) ->
    Printf.sprintf "for %s := %s to %s do %s done" id (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | LetExp(ds, es) ->
    Printf.sprintf "let\n%s\nin\n%s\nend\n"
      (String.concat "\n" (List.map string_of_decl ds))
      (String.concat "\n" (List.map string_of_exp es))
  | Array(typ, size, init) -> Printf.sprintf "%s[%s] of %s" typ (string_of_exp size) (string_of_exp init)
  | Record(typ, fields) ->
    Printf.sprintf "%s { %s }"
      typ
      (String.concat "; "
         (List.map (fun (field, value) -> field ^ "=" ^ string_of_exp value) fields))

and string_of_lvalue = function
  | Ident v -> v
  | ArrayAccess(l, e) -> Printf.sprintf "%s[%s]" (string_of_lvalue l) (string_of_exp e)
  | RecordAccess(l, id) -> string_of_lvalue l ^ "." ^ id

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
  String.concat sep (List.map (fun (v,t) -> v ^ ": " ^ t) tyfields)

and string_of_type_spec = function
  | TypeId i -> i
  | TypeArray i -> "array of " ^ i
  | TypeRecord tyfields -> "{" ^ string_of_tyfields "; " tyfields ^ "}"

and string_of_decl = function
  | VarDecl (i, None, e) -> Printf.sprintf "var %s := %s" i (string_of_exp e)
  | VarDecl (i, Some t, e) -> Printf.sprintf "var %s: %s := %s" i t (string_of_exp e)
  | TypeDecl ty_decls ->
    String.concat "\n"
      (List.map (fun (i, ts) ->
        Printf.sprintf "type %s = %s" i (string_of_type_spec ts))
         ty_decls)
  | FunDecl fun_decls ->
    String.concat "\n"
      (List.map (fun (i, param_list, ty, e) ->
        Printf.sprintf "function %s (%s)%s =\n%s"
          i
          (string_of_tyfields ", " param_list)
          (match ty with
          | None -> ""
          | Some t -> ": " ^ t)
          (string_of_exp e))
         fun_decls)

and string_of_binop op x y =
  string_of_exp x ^ op ^ string_of_exp y

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Lexer.lexer lexbuf in
  print_endline (string_of_exp ast)
