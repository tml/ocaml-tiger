%{

 let make_paren_exp = function
   | [e] -> e
   | es -> Ast.ExpSeq es

 let make_record_access i1 i2 =
   let sym1 = Symbol.symbol i1
   and sym2 = Symbol.symbol i2 in
   Ast.RecordAccess(Ast.Ident sym1, sym2)

 let make_fun_decl i pl o e =
   match o with
  | None -> (Symbol.symbol i, pl, None, e)
  | Some t -> (Symbol.symbol i, pl, Some t, e)

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

%nonassoc Of
%nonassoc Do
%nonassoc Then
%nonassoc Else
%nonassoc ColonEqual
%left Pipe
%left Ampersand
%nonassoc Eq Neq Lt Le Gt Ge
%left Plus Minus
%left Times Div
%nonassoc Uminus

%start <Ast.exp> program

%%


program:
| e=exp Eof { e }
| error { raise Error.Error }

exp:
| Nil { Ast.Nil $startpos }
| Break { Ast.Break }
| x=Int { Ast.Int x }
| Minus x=exp %prec Uminus { Ast.ArithExp (Ast.Sub(Ast.Int 0, x)) }
| s=String { Ast.String s }
| l=lvalue { Ast.LValue l }
| f=funcall { f }
| LParen es=exp_seq RParen { make_paren_exp es }
| ae=arith_exp { Ast.ArithExp ae }
| ce=cmp_exp { Ast.CmpExp ce }
| be=bool_exp { Ast.BoolExp be }
| le=let_exp { le }
| a=assign_stmt { a }
| w=while_stmt { w }
| f=for_stmt { f }
| ite=if_then_else_stmt { ite }
| a=array_exp { a }
| r=record_exp { r }

lvalue:
| i=Ident { Ast.Ident (Symbol.symbol i) }
| i1=Ident Dot i2=Ident { make_record_access i1 i2 }
| i=Ident LBracket e=exp RBracket { Ast.ArrayAccess(Ast.Ident (Symbol.symbol i), e) }
| lv=lvalue LBracket e=exp RBracket { Ast.ArrayAccess(lv, e) }
| lv=lvalue Dot i=Ident { Ast.RecordAccess(lv, Symbol.symbol i) }

funcall:
| i=Ident LParen el=exp_list RParen { Ast.FunCall(Symbol.symbol i, el) }

exp_list:
| el=separated_list(Comma, exp) { el }

exp_seq:
| es=separated_list(SemiColon, exp) { es }


%inline arith_exp:
| e1=exp Plus  e2=exp { Ast.Add(e1, e2) }
| e1=exp Minus e2=exp { Ast.Sub(e1, e2) }
| e1=exp Times e2=exp { Ast.Mul(e1, e2) }
| e1=exp Div   e2=exp { Ast.Div(e1, e2) }

%inline cmp_exp:
| e1=exp Eq  e2=exp { Ast.Eq(e1, e2) }
| e1=exp Neq e2=exp { Ast.Neq(e1, e2) }
| e1=exp Lt  e2=exp { Ast.Lt(e1, e2) }
| e1=exp Le  e2=exp { Ast.Le(e1, e2) }
| e1=exp Gt  e2=exp { Ast.Gt(e1, e2) }
| e1=exp Ge  e2=exp { Ast.Ge(e1, e2) }

%inline bool_exp:
| e1=exp Ampersand e2=exp { Ast.And(e1, e2) }
| e1=exp Pipe e2=exp { Ast.Or(e1, e2) }

let_exp:
| Let ds=decls In le=exp_seq End { Ast.LetExp(ds, le) }

decls:
| ds=list(decl) { ds }

decl:
| vd=var_decl { vd }
| tds=type_decls { tds }
| fds=fun_decls { fds }

var_decl:
| Var i=Ident ColonEqual e=exp { Ast.VarDecl(Symbol.symbol i, None, e) }
| Var i=Ident Colon t=Ident ColonEqual e=exp { Ast.VarDecl(Symbol.symbol i, Some (Symbol.symbol t), e) }

type_decls:
| ds=nonempty_list(type_decl) { Ast.TypeDecl ds }

type_decl:
| Type i=Ident Eq t=type_spec { (Symbol.symbol i, t) }

type_spec:
| i=Ident { Ast.TypeId (Symbol.symbol i) }
| Array Of i=Ident { Ast.TypeArray (Symbol.symbol i) }
| LBrace rt=record_type RBrace { Ast.TypeRecord rt }

record_type:
| { [] }
| v=Ident Colon t=Ident { [(Symbol.symbol v, Symbol.symbol t)] }
| v=Ident Colon t=Ident Comma rt=record_type { (Symbol.symbol v, Symbol.symbol t)::rt }

fun_decls:
| ds=nonempty_list(fun_decl) { Ast.FunDecl ds }

fun_decl:
| Function i=Ident LParen pl=param_list RParen o=option(colon_id) Eq e=exp {
  make_fun_decl i pl o e
}

colon_id: Colon t=Ident { Symbol.symbol t }

param_list:
| pl=separated_list(Comma, param) { pl }

param:
v=Ident Colon t=Ident { (Symbol.symbol v, Symbol.symbol t) }


assign_stmt:
| l=lvalue ColonEqual e=exp { Ast.Assign(l, e) }

while_stmt:
| While e1=exp Do e2=exp { Ast.While(e1, e2) }

for_stmt:
| For i=Ident ColonEqual e1=exp To e2=exp Do e3=exp { Ast.For(Symbol.symbol i, e1, e2, e3) }

if_then_else_stmt:
| If e1=exp Then e2=exp { Ast.IfThen(e1, e2) }
| If e1=exp Then e2=exp Else e3=exp { Ast.IfThenElse(e1, e2, e3) }

array_exp:
| i=Ident LBracket e1=exp RBracket Of e2=exp { Ast.Array(Symbol.symbol i, e1, e2) }

record_exp:
| i=Ident LBrace fields=field_inits RBrace { Ast.Record(Symbol.symbol i, fields) }

field_inits:
| fl=separated_list(Comma, field_init) { fl }

field_init:
| i=Ident Eq e=exp { (Symbol.symbol i, e) }
