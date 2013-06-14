EXT=native

clean:
	ocamlbuild -clean
	rm -f *.byte *.native

test:
	ocamlbuild -use-menhir -package oUnit test_lexer.$(EXT) && ./test_lexer.$(EXT)
