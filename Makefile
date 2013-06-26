EXT=byte

clean:
	ocamlbuild -clean
	rm -f *.byte *.native

test:
	ocamlbuild -use-menhir -package oUnit test_lexer.$(EXT)
	ocamlbuild -use-menhir -package oUnit test_parser.$(EXT)
	ocamlbuild -package oUnit test_symbol.$(EXT)
	./test_lexer.$(EXT)
	./test_parser.$(EXT)
	./test_symbol.$(EXT)

runners:
	ocamlbuild -use-menhir -menhir 'menhir --infer --explain' lexrunner.$(EXT)
	ocamlbuild -use-menhir -menhir 'menhir --infer --explain' parserunner.$(EXT)
