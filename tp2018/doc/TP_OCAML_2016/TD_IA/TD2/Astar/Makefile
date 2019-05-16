COMPILE = ocamlc
COMPILEOPT = ocamlopt
CSLDEP=ocamldep
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLDOC= ocamldoc


NORM_FILES= pqueue.ml memory.ml astar.ml
NORM_OBJS =  $(NORM_FILES:.ml=.cmo)
OPT_OBJS=  $(NORM_FILES:.ml=.cmx)

LIBS= graphics.cma unix.cma

all : findpath findpath.norm doc

doc:
	mkdir doc;$(CAMLDOC) -html -t "TD ASTAR" -d doc -intro Intro -hide Pervasives *.mli

findpath.norm : $(NORM_OBJS) problem.cmo draw.cmo main.cmo
	$(COMPILE) -o $@ $(LIBS) $(NORM_OBJS) problem.cmo draw.cmo main.cmo

findpath : $(OPT_OBJS) problem.cmx draw.cmx main.cmx
	$(COMPILEOPT) -o $@ $(LIBS:.cma=.cmxa) $(OPT_OBJS) problem.cmx draw.cmx main.cmx

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.mll.mli:
	$(CAMLLEX) $<
.mll.ml:
	$(CAMLLEX) $<
.mly.mli:
	$(CAMLYACC) $<
.mly.ml:
	$(CAMLYACC) $<
.ml.cmo :
	$(COMPILE) -c $<
.mli.cmi :
	$(COMPILE) -c $<
.ml.cmx :
	$(COMPILEOPT) -c $<

cleandoc:
	\rm -Rf doc

clean:
	\rm -f main.cmo main.cmi main.cmx astar.cmo astar.cmi astar.cmx .depend findpath.norm findpath

.depend:
	$(CSLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend
