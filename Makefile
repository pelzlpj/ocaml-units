all: units.cmo constants.cmo

units.cmo: units.ml
	ocamlc -c units.ml

constants.cmo: constants.ml
	ocamlc -c constants.ml

clean:
	rm -f *.cmo *.cmi *.mli




# arch-tag: DO_NOT_CHANGE_0bfec0da-df85-45c5-8615-10d4efc283c6 
