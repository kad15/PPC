
SOURCES = arbre.ml

TARGET = arbre

DEBUG =   # -g

OCAML = ocamlfind opt $(DEBUG)

LIBS = -package dot

LDFLAGS = -linkpkg

DEP = ocamldep

OBJS = $(SOURCES:.ml=.cmx)

all: .depend byte
 
byte: $(TARGET)

$(TARGET): $(OBJS)
	$(OCAML) $(LIBS) $(LDFLAGS) -o $@ $^
	
%.cmi: %.mli
	$(OCAML) $(LIBS) $<
	
%.cmx: %.ml
	$(OCAML) $(LIBS) -c $<
	
.PHONY: clean

clean:
	rm -f *.cm[iox] *.o *~ .*~ 
	rm -f $(TARGET)

.depend: $(SOURCES)
	$(DEP) *.mli *.ml > .depend

include .depend
