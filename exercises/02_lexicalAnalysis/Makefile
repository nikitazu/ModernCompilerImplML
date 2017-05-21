all: main.native

test: main.native
	echo 'var a_2 = 1/2; if a <> 2 then 123 else 4. or .05 end' | ./main.native

main.native: main.ml
	ocamlbuild main.native

clean:
	rm -rf _build main.native
