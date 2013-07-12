type venv = Env.env_entry Symbol.table
type tenv = Env.ty Symbol.table
type expty = {exp: Translate.exp; ty: Types.t}

val translate_exp       : venv -> tenv -> Ast.exp       -> expty
val translate_arith_exp : venv -> tenv -> Ast.arith_exp -> expty
val translate_bool_exp  : venv -> tenv -> Ast.bool_exp  -> expty
val translate_cmp_exp   : venv -> tenv -> Ast.cmp_exp   -> expty
val translate_lvalue    : venv -> tenv -> Ast.lvalue    -> expty
val translate_decl      : venv -> tenv -> Ast.lvalue    -> (venv * tenv)
val translate_type      :         tenv -> Ast.type_spec -> Types.t
