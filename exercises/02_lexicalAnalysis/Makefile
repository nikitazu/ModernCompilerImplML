all: main.native

test: main.native
	echo foo bar 123 if | ./main.native

main.native: main.ml
	ocamlbuild main.native

clean:
	rm -rf _build main.native
