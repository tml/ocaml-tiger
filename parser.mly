%{

  let pos p =
    Printf.sprintf "%d:%d" p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

 let make_paren_exp = function
   | [e] -> e
   | es -> Ast.ExpSeq es

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
| Nil { Ast.Nil }
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
| i=Ident { Ast.Ident i }
| i1=Ident Dot i2=Ident { Ast.RecordAccess(Ast.Ident i1, i2) }
| i=Ident LBracket e=exp RBracket { Ast.ArrayAccess(Ast.Ident i, e) }
| lv=lvalue LBracket e=exp RBracket { Ast.ArrayAccess(lv, e) }
| lv=lvalue Dot i=Ident { Ast.RecordAccess(lv, i) }

funcall:
| i=Ident LParen el=exp_list RParen { Ast.FunCall(i, el) }

exp_list:
| { [] }
| e=exp { [e] }
| e=exp Comma el=exp_list { e::el }

exp_seq:
| { [] }
| e=exp { [e] }
| e=exp SemiColon es = exp_seq { e::es }


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
| Var i=Ident ColonEqual e=exp { Ast.VarDecl(i, None, e) }
| Var i=Ident Colon t=Ident ColonEqual e=exp { Ast.VarDecl(i, Some t, e) }

type_decls:
| ds=nonempty_list(type_decl) { Ast.TypeDecl ds }

type_decl:
| Type i=Ident Eq t=type_spec { (i, t) }

type_spec:
| i=Ident { Ast.TypeId i }
| Array Of i=Ident { Ast.TypeArray i }
| LBrace rt=record_type RBrace { Ast.TypeRecord rt }

record_type:
| { [] }
| v=Ident Colon t=Ident { [(v, t)] }
| v=Ident Colon t=Ident SemiColon rt=record_type { (v,t)::rt }

fun_decls:
| ds=nonempty_list(fun_decl) { Ast.FunDecl ds }

fun_decl:
| Function i=Ident LParen pl=param_list RParen o=option(colon_id) Eq e=exp {
  match o with
  | None -> (i, pl, None, e)
  | Some t -> (i, pl, Some t, e)
}

colon_id: Colon t=Ident { t }

param_list:
| { [] }
| v=Ident Colon t=Ident { [(v, t)] }
| v=Ident Colon t=Ident Comma pl=param_list { (v,t)::pl }


assign_stmt:
| l=lvalue ColonEqual e=exp { Ast.Assign(l, e) }

while_stmt:
| While e1=exp Do e2=exp { Ast.While(e1, e2) }

for_stmt:
| For i=Ident ColonEqual e1=exp To e2=exp Do e3=exp { Ast.For(i, e1, e2, e3) }

if_then_else_stmt:
| If e1=exp Then e2=exp { Ast.IfThen(e1, e2) }
| If e1=exp Then e2=exp Else e3=exp { Ast.IfThenElse(e1, e2, e3) }

array_exp:
| i=Ident LBracket e1=exp RBracket Of e2=exp { Ast.Array(i, e1, e2) }

record_exp:
| i=Ident LBrace fields=field_inits RBrace { Ast.Record(i, fields) }

field_inits:
| { [] }
| i=Ident Eq e=exp { [(i, e)] }
| i=Ident Eq e=exp SemiColon rest=field_inits { (i, e)::rest }
