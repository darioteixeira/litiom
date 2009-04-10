NAME=module

all: $(NAME).cma $(NAME).cmxs

%.cma: %.cmo
	ocamlc -a -o $@ $+

%.cmxa: %.cmx
	ocamlopt -a -o $@ $+

%.cmxs: %.cmxa
	ocamlopt -shared -linkall -I `pwd` -o $@ $<

%.cmo: %.ml
	ocamlfind ocamlc -thread -package lwt,ocsigen,litiom -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -thread -package lwt,ocsigen,litiom -c $<

clean:
	rm -f $(NAME).cm[ioax] $(NAME).cmx[as] $(NAME).[oa]

