all: main.native

test: main.native
	cat program.tig | ./main.native

main.native: tokens.mli tokens.ml lexer.mll main.ml
	ocamlbuild main.native

clean:
	rm -rf _build main.native
