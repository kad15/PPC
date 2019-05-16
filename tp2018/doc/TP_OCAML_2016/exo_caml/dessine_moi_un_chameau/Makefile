SOURCES = camel.ml
EXEC = camel
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
LIBS=graphics.cmxa

all: depend $(EXEC)

OBJS = $(SOURCES:.ml=.cmx)

$(EXEC): $(OBJS)
	$(CAMLOPT) -o $(EXEC) $(LIBS) $(OBJS)

.SUFFIXES: .ml .mli .cmi .cmx

.mli.cmi:
	$(CAMLOPT) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o #*#
	rm -f $(EXEC)

.depend: $(SOURCES)
	$(CAMLDEP) *.mli *.ml > .depend

depend: $(SOURCES)
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

