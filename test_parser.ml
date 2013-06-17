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
       [try (ignore (parse_string "x[0]()"); false) with Error.Error -> true;
        try (ignore (parse_string "x.y()"); false) with Error.Error -> true;
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
  ]

let _ =
  run_test_tt_main suite
