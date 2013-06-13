%{

%}

/* Keywords */
%token Let In End Var Type Function
%token Array Of Nil
%token If Then Else For To Do While Break

/* Punctuation */
%token LParen RParen LBracket RBracket LBrace RBrace
%token Plus Minus Times Div Ampersand Pipe
%token Eq Neq Lt Le Gt Ge
%token Dot Comma Colon SemiColon ColonEqual

%token <int> Int
%token <string> String
%token <string> Ident

%token Eof

%start <Ast.exp> prgm

%%


prgm:
| _ = Eof { Ast.Nil }
