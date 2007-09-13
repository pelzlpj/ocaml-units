all: units.cmo

test: test.ml units.cmx
	ocamlopt -o test str.cmxa units.cmx test.ml

units.cmo: units.ml
	ocamlc -c units.ml

units.cmx: units.ml
	ocamlopt -c units.ml

clean:
	rm -f *.cmo *.cmi *.mli test




# arch-tag: DO_NOT_CHANGE_0bfec0da-df85-45c5-8615-10d4efc283c6 
