open OUnit
open Ast

let parse_string str =
  Parser.program Lexer.lexer (Lexing.from_string str)

let test_string () =
  assert_equal [parse_string "\"\""; parse_string "\"foo\""] [String ""; String "foo"]

let test_zero () = assert_equal (parse_string "0") (Int 0)
let test_positive () = assert_equal (parse_string "1") (Int 1)
let test_negative () = assert_equal (parse_string "-1") (ArithExp (Sub (Int 0, Int 1)))

let test_nil () = assert_equal (parse_string "nil") Nil
let test_break () = assert_equal (parse_string "break") Break

let test_funcall () =
  assert_equal [parse_string "f()"; parse_string "f(1)"; parse_string "f(1,x)"]
    [FunCall("f", []); FunCall("f", [Int 1]); FunCall("f", [Int 1; LValue(Ident "x")])]

let test_invalid_funcall () =
  (* Can only use identifiers for function calls, not lvalues such as
     array accesses and record accesses. *)
  assert_bool "test_invalid_funcall"
    (List.for_all (fun x -> x)
       [(try (ignore (parse_string "x[0]()"); false) with Error.Error -> true);
        (try (ignore (parse_string "x.y()"); false) with Error.Error -> true);
       ])

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
  (* Comparison is not associative *)
  assert_equal
    (List.map parse_string ["1<2"; "x<=1"; "1=1"; "e>0"; "3>=2"]
     @ [(try (ignore (parse_string "1<2<3"); Int 1) with Error.Error -> Int 0);
        (try parse_string "1<(2<3)" with Error.Error -> Int 0)]
    )
    [CmpExp(Lt(Int 1, Int 2));
     CmpExp(Le(LValue(Ident "x"), Int 1));
     CmpExp(Eq(Int 1, Int 1));
     CmpExp(Gt(LValue(Ident "e"), Int 0));
     CmpExp(Ge(Int 3, Int 2));
     Int 0;
     CmpExp(Lt(Int 1, CmpExp(Lt(Int 2, Int 3))))]


let suite =
  "parser suite" >::: [
    "test_zero" >:: test_zero;
    "test_positive" >:: test_positive;
    "test_negative" >:: test_negative;

    "test_nil" >:: test_nil;
    "test_break" >:: test_break;

    "test_string" >:: test_string;

    "test_funcall" >:: test_funcall;
    "test_invalid_funcall" >:: test_invalid_funcall;

    "test_lvalue" >:: test_lvalue;
    "test_arith" >:: test_arith;
    "test_cmp" >:: test_cmp;
    "test_parens" >:: test_parens;

  ]

let _ =
  run_test_tt_main suite
