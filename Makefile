OCAMLBUILS_OPTS:=-classic-display

all: lib

lib:
	ocamlbuild $(OCAMLBUILS_OPTS) litiom.cma


install: lib
	ocamlfind remove litiom
	ocamlfind install litiom META _build/litiom.cma _build/litiom_blocks.cmi _build/litiom_wizard.cmi

doc: lib
	mkdir -p html
	ocamlfind ocamldoc -package lwt,ocsigen -v -html -keep-code -d html -I _build/ \
	litiom_blocks.mli litiom_blocks.ml \
	litiom_wizard.mli litiom_wizard.ml


clean:
	ocamlbuild $(OCAMLBUILS_OPTS) -clean

