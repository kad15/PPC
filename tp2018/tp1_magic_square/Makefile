# Generic Makefile for single files using FaCiLe. Allow to produce
# file.out (ocamlc) and file.opt (ocamlopt) from file.ml while linking
# with the FaCiLe library

FACILEDIR= ~/ppc/facile
INCLUDES= -I $(FACILEDIR)

LIBS = facile.cma unix.cma
LIBSOPT = $(LIBS:.cma=.cmxa)

OCAMLC= ocamlc -g $(INCLUDES)
OCAMLMLI= ocamlc $(INCLUDES)
OCAMLOPT= ocamlopt -unsafe -noassert -inline 100 $(INCLUDES)
OCAMLDEP= ocamldep $(INCLUDES)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmi .cmo .cmx .out .opt

.ml.cmo :
	$(OCAMLC) -c $<
.mli.cmi :
	$(OCAMLMLI) -c $<
.ml.cmx :
	$(OCAMLOPT) -c $<
.cmo.out :
	$(OCAMLC) -o $@ $(LIBS) $<
.cmx.opt :
	$(OCAMLOPT) -o $@ $(LIBSOPT) $<

clean:
	\rm -f *.cmo *.cmi *.cmx *.o *~ *.opt *.out .depend

.depend:
	$(OCAMLDEP) *.mli *.ml > $@

include .depend

#.PHONY: auto-ocp-indent
#auto-ocp-indent: *.ml
#    for mlfile in $^; do ocp-indent -i $$mlfile; done
