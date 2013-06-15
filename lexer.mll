{
 open Parser
 exception LexingError of string
}

let alpha = ['a' - 'z' 'A' - 'Z' '_']
let digit = ['0' - '9']

rule lexer = parse
  | eof { Eof }
  | "/*" { comment 0 lexbuf }

      (* keywords *)
  | "let" { Let }
  | "in" { In }
  | "end" { End }
  | "type" { Type }
  | "var" { Var }
  | "array" { Array }
  | "of" { Of }
  | "function" { Function }
  | "for" { For }
  | "to" { To }
  | "do" { Do }
  | "done" { Done }
  | "while" { While }
  | "if" { If }
  | "then" { Then }
  | "else" { Else }
  | "nil" { Nil }
  | "break" { Break }
      (* punctuation *)
  | "(" { LParen }
  | ")" { RParen }
  | "[" { LBracket }
  | "]" { RBracket }
  | "{" { LBrace }
  | "}" { RBrace }
  | ":" { Colon }
  | ";" { SemiColon }
  | ":=" { ColonEqual }
  | "+" { Plus }
  | "-" { Minus }
  | "*" { Times }
  | "/" { Div }
  | "&" { Ampersand }
  | "|" { Pipe }
  | "=" { Eq }
  | "<>" { Neq }
  | "<" { Lt }
  | "<=" { Le }
  | ">" { Gt }
  | ">=" { Ge }
  | "." { Dot }
  | "," { Comma }

  | alpha (alpha | digit)* as v { Ident v }
  | digit+ as n { Int (int_of_string n) }
  | "\"" ([^ '"']* as s) "\""  { String s }

  | [' ' '\t' '\n']+ { lexer lexbuf }
  | _ as c { raise (LexingError ("unknown character: " ^ (String.make 1 c))) }

and comment depth = parse
  | "*/" { if depth = 0 then lexer lexbuf else comment (depth-1) lexbuf }
  | "/*" { comment (depth+1) lexbuf }
  | eof { raise (LexingError "unclosed comment") }
  | _ { comment depth lexbuf }
