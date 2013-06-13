clean:
	ocamlbuild -clean
	rm -f *.byte *.native

test:
	ocamlbuild -use-menhir -package oUnit test_lexer.byte && ./test_lexer.byte
