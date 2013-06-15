open Parser

let string_of_token = function
  | Ident s -> "Ident(" ^ s ^ ")"
  | Int n -> "Int(" ^ string_of_int n ^ ")"
  | String s -> "String(" ^ s ^ ")"
        (* keywords *)
  | Let -> "Let"
  | In -> "In"
  | End -> "End"
  | Var -> "Var"
  | Type -> "Type"
  | Array -> "Array"
  | Of -> "Of"
  | Function -> "Function"
  | For -> "For"
  | To -> "To"
  | Do -> "Do"
  | Done -> "Done"
  | While -> "While"
  | If -> "If"
  | Then -> "Then"
  | Else -> "Else"
  | Nil -> "Nil"
  | Break -> "break"
      (* Punctuation *)
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LBracket -> "LBracket"
  | RBracket -> "RBracket"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | Colon -> "Colon"
  | SemiColon -> "SemiColon"
  | ColonEqual -> "ColonEqual"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Div -> "Div"
  | Ampersand -> "Ampersand"
  | Pipe -> "Pipe"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | Dot -> "Dot"
  | Comma -> "Comma"

  | Eof -> "Eof"


let rec print_code lexbuf =
  let t = Lexer.lexer lexbuf in
  Printf.printf "%s\n" (string_of_token t);
  if t <> Eof then print_code lexbuf


let _ =
  let lexbuf = Lexing.from_channel stdin in
  print_code lexbuf
