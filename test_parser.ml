open OUnit
open Ast

let parse_string str =
  Parser.program Lexer.lexer (Lexing.from_string str)

let is_valid_parse str =
  try
    ignore (parse_string str);
    true
  with Error.Error -> false

let test_string () =
  assert_equal [parse_string "\"\""; parse_string "\"foo\""] [String ""; String "foo"]

let test_int () =
  assert_equal
    (List.map parse_string ["0"; "1"; "-1"])
    [Int 0; Int 1; ArithExp(Sub(Int 0, Int 1))]

let test_nil () = assert_equal (parse_string "nil") Nil
let test_break () = assert_equal (parse_string "break") Break

let test_funcall () =
  assert_equal
    (List.map parse_string ["f()"; "f(1)"; "f(1,x)"])
    [FunCall("f", []);
     FunCall("f", [Int 1]);
     FunCall("f", [Int 1; LValue(Ident "x")])]

let test_invalid_funcall () =
  (* Can only use identifiers for function calls, not lvalues such as
     array accesses and record accesses. *)
  assert_bool "test_invalid_funcall"
    (List.for_all (fun x -> not (is_valid_parse x))
       ["x[0]()"; "x.y()"])

let test_lvalue () =
  assert_equal
    (List.map parse_string ["x"; "x.y"; "x.y.z"; "x[0]"; "x[a+b]";
                            "x[0].y"; "x.y[0]"])
    [LValue(Ident "x");
     LValue(RecordAccess(Ident "x", "y"));
     LValue(RecordAccess(RecordAccess(Ident "x", "y"), "z"));
     LValue(ArrayAccess(Ident "x", Int 0));
     LValue(ArrayAccess(Ident "x", ArithExp(Add(LValue(Ident "a"), LValue(Ident "b")))));
     LValue(RecordAccess(ArrayAccess(Ident "x", Int 0), "y"));
     LValue(ArrayAccess(RecordAccess(Ident "x", "y"), Int 0));
    ]


let test_arith () =
  assert_equal
    (List.map parse_string ["2+3"; "2+3-4"; "2*3+4"; "2+3/4"; "2*-3"; "(2+3)*4"])
    [ArithExp(Add(Int 2, Int 3));
     ArithExp(Sub(ArithExp(Add(Int 2, Int 3)), Int 4));
     ArithExp(Add(ArithExp(Mul(Int 2, Int 3)), Int 4));
     ArithExp(Add(Int 2, ArithExp(Div(Int 3, Int 4))));
     ArithExp(Mul(Int 2, ArithExp(Sub(Int 0, Int 3))));
     ArithExp(Mul(ArithExp(Add(Int 2, Int 3)), Int 4));
    ]

let test_parens () =
  assert_equal
    (List.map parse_string ["()"; "(1)"; "(1; 2)"])
    [ExpSeq []; Int 1; ExpSeq [Int 1; Int 2]]

let test_cmp () =
  assert_equal
    (List.map parse_string ["1<2"; "x<=1"; "1=1"; "e>0"; "3>=2"])
    [CmpExp(Lt(Int 1, Int 2));
     CmpExp(Le(LValue(Ident "x"), Int 1));
     CmpExp(Eq(Int 1, Int 1));
     CmpExp(Gt(LValue(Ident "e"), Int 0));
     CmpExp(Ge(Int 3, Int 2));
    ]

let test_cmp_assoc () =
  assert_equal
    [not (is_valid_parse "1<2<3"); is_valid_parse "1<(2<3)"]
    [true; true]

let test_bool () =
  assert_equal
    (List.map parse_string
       ["1 & 1";
        "1 | 1";
        "1 | 2 & 3";
        "1 & 2 | 3";
       ])
    [BoolExp(And(Int 1, Int 1));
     BoolExp(Or(Int 1, Int 1));
     BoolExp(Or(Int 1, BoolExp(And(Int 2, Int 3))));
     BoolExp(Or(BoolExp(And(Int 1, Int 2)), Int 3));
    ]

let test_assign () =
  assert_equal
    (List.map parse_string
       ["a := 1";
        "a.b := 1";
        "a[0] := 1";
        "a := (b := 1)";
       ])
    [Assign(Ident "a", Int 1);
     Assign(RecordAccess(Ident "a", "b"), Int 1);
     Assign(ArrayAccess(Ident "a", Int 0), Int 1);
     Assign(Ident "a", Assign(Ident "b", Int 1));
    ]

let test_let () =
  assert_equal
    (List.map parse_string
       ["let in end";
        "let in 1 end";
        "let var x: int := 1 in x end";
        "let var x := 1 in x end";
        "let type t = int in 1 end";
        "let type t = {x:int} in 1 end";
        "let type t = array of int in 1 end";
        "let function f() = 0 in 1 end";
        "let function f(): int = 0 in 1 end";
        "let function f(x:int) = x in 1 end";
        "let function f(x:int, y:int): int = x+y in 1 end";
        "let function a() = 1 function b() = 2 in 1 end";
        "let function a() = 1 var b := 2 function c() = 3 in 1 end";
       ])
    [LetExp([], []);
     LetExp([], [Int 1]);
     LetExp([VarDecl("x", Some "int", Int 1)], [LValue(Ident "x")]);
     LetExp([VarDecl("x", None, Int 1)], [LValue(Ident "x")]);
     LetExp([TypeDecl("t", TypeId("int"))], [Int 1]);
     LetExp([TypeDecl("t", TypeRecord [("x", "int")])], [Int 1]);
     LetExp([TypeDecl("t", TypeArray "int")], [Int 1]);
     LetExp([FunDecl([("f", [], None, Int 0)])], [Int 1]);
     LetExp([FunDecl([("f", [], Some "int", Int 0)])], [Int 1]);
     LetExp([FunDecl([("f", [("x", "int")], None, LValue(Ident "x"))])], [Int 1]);
     LetExp([FunDecl([("f", [("x", "int"); ("y", "int")], Some "int",
                     ArithExp(Add(LValue(Ident "x"), LValue(Ident "y"))))])],
            [Int 1]);
     LetExp([FunDecl [("a", [], None, Int 1);
                      ("b", [], None, Int 2)]], [Int 1]);
     LetExp([FunDecl [("a", [], None, Int 1)];
             VarDecl ("b", None, Int 2);
             FunDecl [("c", [], None, Int 3)]], [Int 1]);

    ]



let test_invalid_let () =
  assert_bool "test_invalid_let"
    (List.for_all (fun x -> not (is_valid_parse x))
       ["let in";
        "let in 1";
        "let 3 in end";
        "let var x = 3 in x end";
        "let type t in 3 end";
       ])

let test_if () =
  assert_equal
    (List.map parse_string
       ["if 0 then 1";
        "if 0 then 1 else 2";
        "if 0 then 1 else if 2 then 3 else 4";
        "if 0 then if 1 then 2 else 3 else 4";
        "if 0 then if 1 then 2 else 3";
       ])
    [IfThen(Int 0, Int 1);
     IfThenElse(Int 0, Int 1, Int 2);
     IfThenElse(Int 0, Int 1, IfThenElse(Int 2, Int 3, Int 4));
     IfThenElse(Int 0, IfThenElse(Int 1, Int 2, Int 3), Int 4);
     IfThen(Int 0, IfThenElse(Int 1, Int 2, Int 3));
    ]

let test_loop () =
  assert_equal
    (List.map parse_string
       ["while 0 do 1";
        "for i := 0 to 1 do 2";
       ])
    [While(Int 0, Int 1);
     For("i", Int 0, Int 1, Int 2);
    ]

let test_aggregate_expressions () =
  assert_equal
    (List.map parse_string
       ["int[3] of 0";
        "int[3] of int[3] of 0";
        "empty {}";
        "point {x=3; y=4}";
       ])
    [Array("int", Int 3, Int 0);
     Array("int", Int 3, Array("int", Int 3, Int 0));
     Record("empty", []);
     Record("point", [("x", Int 3); ("y", Int 4)]);
    ]


let suite =
  "parser suite" >::: [
    "test_nil" >:: test_nil;
    "test_break" >:: test_break;

    "test_int" >:: test_int;
    "test_string" >:: test_string;

    "test_funcall" >:: test_funcall;
    "test_invalid_funcall" >:: test_invalid_funcall;

    "test_lvalue" >:: test_lvalue;
    "test_arith" >:: test_arith;
    "test_cmp" >:: test_cmp;
    "test_cmp_assoc" >:: test_cmp_assoc;
    "test_bool" >:: test_bool;
    "test_parens" >:: test_parens;

    "test_let" >:: test_let;
    "test_invalid_let" >:: test_invalid_let;
    "test_assign" >:: test_assign;

    "test_if" >:: test_if;
    "test_loop" >:: test_loop;

    "test_aggregate_expressions" >:: test_aggregate_expressions;
  ]

let _ =
  run_test_tt_main suite
