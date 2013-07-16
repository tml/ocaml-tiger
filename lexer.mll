{
 open Parser
 exception LexingError of string

 let newline lexbuf =
   let pos = lexbuf.Lexing.lex_curr_p in
   lexbuf.Lexing.lex_curr_p <-
     {pos with
       Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
       Lexing.pos_bol = pos.Lexing.pos_cnum
     }

 let line pos =
   pos.Lexing.pos_lnum

 let col pos =
   pos.Lexing.pos_cnum - pos.Lexing.pos_bol

 let error lexbuf msg =
   let reason = Printf.sprintf "%s (%d, %d)"
     msg
     lexbuf.Lexing.lex_curr_p.pos_lnum
     lexbuf.Lexing.lex_curr_p.pos_cnum in
   raise (LexingError reason)

}

let alpha = ['a' - 'z' 'A' - 'Z' '_']
let digit = ['0' - '9']

rule lexer = parse
  | eof        { Eof }
  | "/*"       { comment 0 lexbuf }

      (* keywords *)
  | "let"      { Let }
  | "in"       { In }
  | "end"      { End }
  | "type"     { Type }
  | "var"      { Var }
  | "array"    { Array }
  | "of"       { Of }
  | "function" { Function }
  | "for"      { For }
  | "to"       { To }
  | "do"       { Do }
  | "while"    { While }
  | "if"       { If }
  | "then"     { Then }
  | "else"     { Else }
  | "nil"      { Nil }
  | "break"    { Break }
      (* punctuation *)
  | "("        { LParen }
  | ")"        { RParen }
  | "["        { LBracket }
  | "]"        { RBracket }
  | "{"        { LBrace }
  | "}"        { RBrace }
  | ":"        { Colon }
  | ";"        { SemiColon }
  | ":="       { ColonEqual }
  | "+"        { Plus }
  | "-"        { Minus }
  | "*"        { Times }
  | "/"        { Div }
  | "&"        { Ampersand }
  | "|"        { Pipe }
  | "="        { Eq }
  | "<>"       { Neq }
  | "<"        { Lt }
  | "<="       { Le }
  | ">"        { Gt }
  | ">="       { Ge }
  | "."        { Dot }
  | ","        { Comma }
  | alpha (alpha | digit)* as v { Ident v }
  | digit+ as n                 { Int (int_of_string n) }
  | "\"" ([^ '"']* as s) "\""   { String s } (* TODO: better string handling. *)
  | [' ' '\t']+                 { lexer lexbuf }
  | "\n"                        { newline lexbuf; lexer lexbuf }
  | _ as c { error lexbuf ("unknown character: " ^ (String.make 1 c)) }

and comment depth = parse
  | eof   { error lexbuf "unclosed comment" }
  | "*/"  { if depth = 0 then lexer lexbuf else comment (depth-1) lexbuf }
  | "/*"  { comment (depth+1) lexbuf }
  | "\n"  { newline lexbuf; comment depth lexbuf }
  | _     { comment depth lexbuf }
