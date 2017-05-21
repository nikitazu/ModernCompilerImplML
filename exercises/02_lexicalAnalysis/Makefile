all: main.native

test: main.native
	echo 'if a then 123 else 4. or .05 end' | ./main.native

main.native: main.ml
	ocamlbuild main.native

clean:
	rm -rf _build main.native
