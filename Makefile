EXT=native

clean:
	ocamlbuild -clean
	rm -f *.byte *.native

test:
	ocamlbuild -use-menhir -package oUnit test_lexer.$(EXT) && ./test_lexer.$(EXT)

runners:
	ocamlbuild -use-menhir lexrunner.$(EXT)
	ocamlbuild -use-menhir parserunner.$(EXT)
