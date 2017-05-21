all: main.native

test: main.native
	cat program.tig | ./main.native

main.native: main.ml
	ocamlbuild main.native

clean:
	rm -rf _build main.native
