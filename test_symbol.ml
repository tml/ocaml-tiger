open OUnit

let test_symbol () =
  assert_equal (Symbol.name (Symbol.symbol "hello")) "hello"

let test_symbol_equality () =
  let a = Symbol.symbol "a" in
  let a' = Symbol.symbol "a" in
  assert_equal a a'

let test_get () =
  let symtable = Symbol.empty in
  let symtable = Symbol.put symtable (Symbol.symbol "a") "int" in
  let symtable = Symbol.put symtable (Symbol.symbol "b") "string" in

  assert_equal
    [Symbol.get symtable (Symbol.symbol "a");
     Symbol.get symtable (Symbol.symbol "b");
     Symbol.get symtable (Symbol.symbol "c");
    ]
    [Some "int";
     Some "string";
     None;
    ]

let test_put () =
  let symtable = Symbol.empty in
  let symtable = Symbol.put symtable (Symbol.symbol "a") "int" in
  let symtable = Symbol.put symtable (Symbol.symbol "a") "string" in
  assert_equal
    [Symbol.get symtable (Symbol.symbol "a")]
    [Some "string"]

let suite =
  "symbol table suite" >::: [
    "test_symbol" >:: test_symbol;
    "test_symbol_equality" >:: test_symbol_equality;
    "test_get" >:: test_get;
    "test_put" >:: test_put;
  ]

let _ =
  run_test_tt_main suite
