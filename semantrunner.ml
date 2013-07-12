let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Lexer.lexer lexbuf in
  let {Semant.exp=exp; Semant.ty=ty} = Semant.translate_exp Env.base_venv Env.base_tenv ast in
  Printf.printf "%b %b\n" (exp = Translate.Undef) (ty = Types.Int)
