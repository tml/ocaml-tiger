open OUnit
open Parser

let lex_string str =
  let lexbuf = Lexing.from_string str in
  let rec loop () =
    let token = Lexer.lexer lexbuf in
    if token = Eof then
      []
    else
      token :: loop ()
  in
  loop ()

let test_comment () = assert_equal (lex_string "/* 1 */") []
let test_nested_comment () = assert_equal (lex_string "/* 1 /* 2 */ 1 */") []

let test_int_zero () = assert_equal (lex_string "0") [Int 0]
let test_int_positive () = assert_equal (lex_string "1") [Int 1]
let test_int_negative () = assert_equal (lex_string "-1") [Minus; Int 1]

let test_empty_string () = assert_equal (lex_string "\"\"") [String ""]
let test_string () = assert_equal (lex_string "\"foo\"") [String "foo"]
(* let test_string_escape1 () = assert_equal (lex_string "\"\\\"\"") [String "\\\""] *)

let test_identifiers () =
  assert_equal (lex_string "foo Foo _foo foo1 x")
    [Ident "foo"; Ident "Foo"; Ident "_foo"; Ident "foo1"; Ident "x"]

let test_invalid_identifiers () =
  assert_equal (lex_string "2a") [Int 2; Ident "a"]

let test_keywords () =
  assert_equal (lex_string ("let in end type var array of function for " ^
                            "to do while if then else nil break"))
    [Let; In; End; Type; Var; Array; Of; Function; For;
     To; Do; While; If; Then; Else; Nil; Break]

let test_punctuation1 () =
  assert_equal (lex_string ("( ) [ ] { } : ; , . + - * / & | " ^
                            "= <> < <= > >= :="))
    [LParen; RParen; LBracket; RBracket; LBrace; RBrace;
     Colon; SemiColon; Comma; Dot; Plus; Minus; Times; Div;
     Ampersand; Pipe; Eq; Neq; Lt; Le; Gt; Ge; ColonEqual]

let test_punctuation2 () =
  assert_equal (lex_string ("()[]{}:;,.+-*/&|=<><<=>>=:="))
    [LParen; RParen; LBracket; RBracket; LBrace; RBrace;
     Colon; SemiColon; Comma; Dot; Plus; Minus; Times; Div;
     Ampersand; Pipe; Eq; Neq; Lt; Le; Gt; Ge; ColonEqual]

let test_invalid_punctuation () =
  let is_invalid_char s =
    try (lex_string s; false)
    with Lexer.LexingError _ -> true
  in

  assert_bool "invalid punctuation"
    (List.fold_left
       (fun acc c -> acc && is_invalid_char c)
       true
       ["#"; "!"; "@"; "$"; "%"; "^"])



let suite =
  "lex suite" >::: [
    "test_comment" >:: test_comment;
    "test_nested_comment" >:: test_nested_comment;
    "test_int_zero" >:: test_int_zero;
    "test_int_positive" >:: test_int_positive;
    "test_int_negative" >:: test_int_negative;
    "test_empty_string" >:: test_empty_string;
    "test_string" >:: test_string;
    (* "test_string_escape1" >:: test_string_escape1; *)
    "test_identifiers" >:: test_identifiers;
    "test_invalid_identifiers" >:: test_invalid_identifiers;
    "test_keywords" >:: test_keywords;
    "test_punctuation1" >:: test_punctuation1;
    "test_punctuation2" >:: test_punctuation2;
    "test_invalid_punctuation" >:: test_invalid_punctuation;
  ]

let _ =
  run_test_tt_main suite
