# $Id: Makefile.in,v 1.1 2006-02-02 17:22:26 barnier Exp $
OCAMLC = ocamlc.opt
OCAMLMLI = ocamlc.opt
OCAMLOPT = ocamlopt.opt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex.opt
OCAMLYACC = ocamlyacc
OCAMLTOP = ocamlmktop

OCAMLCFLAGS = -g
OCAMLOPTFLAGS = -unsafe -noassert -inline 100

FILIBOCAML_DIR = fi_ocaml

FINCLUDES = -I $(FILIBOCAML_DIR) -cclib -linterval interval.cma 
FINCLUDES_OPT = $(FINCLUDES:.cma=.cmxa)

OPTOPT = 
OPT_OPTIONS =

OCAMLLIB = /usr/lib/ocaml
FACILELIB = $(OCAMLLIB)/facile

CSTR =	fcl_debug.ml fcl_misc.ml fcl_domain.ml fcl_setDomain.ml fcl_stak.ml \
	fcl_data.ml fcl_weakstore.ml fcl_cstr.ml fcl_var.ml fcl_invariant.ml \
	fcl_reify.ml fcl_boolean.ml fcl_linear.ml fcl_nonlinear.ml \
	fcl_expr.ml fcl_arith.ml fcl_conjunto.ml fcl_goals.ml \
	fcl_fdArray.ml fcl_gcc.ml fcl_opti.ml fcl_alldiff.ml fcl_sorting.ml \
	fcl_interval.ml	fcl_genesis.ml fcl_binary.ml fcl_ac6.ml facile.ml

CSTRF =	fcl_float_interval.ml fcl_float_domain.ml fcl_float_var.ml \
	fcl_float_cstr.ml fcl_float_arith.ml fcl_float_goals.ml \
	fcl_float_array.ml float_Facile.ml

CSTRCMO = $(CSTR:.ml=.cmo)
CSTRCMX = $(CSTR:.ml=.cmx)

CSTRFCMO =  $(CSTRF:.ml=.cmo)
CSTRFCMX = $(CSTRF:.ml=.cmx)

# Default target: libraries and toplevel
all : all_facile

all_facile_byte : facile facile.cma

all_facile : facile facile.cma facile.cmxa

all_float_facile_byte : facile facile.cma interval_byte float_facile float_facile.cma

all_float_facile : facile facile.cma facile.cmxa interval float_facile float_facile.cma float_facile.cmxa

fcl_float_interval.cmo : INCLUDES=$(FINCLUDES) 

fcl_float_interval.cmx : INCLUDES_OPT=$(FINCLUDES_OPT)

# Libraries
facile.cma : $(CSTRCMO)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ -a  $(CSTRCMO)

facile.cmxa : $(CSTRCMX)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -a $(CSTRCMX)

float_facile.cma :  $(CSTRFCMO)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ -a  $(CSTRFCMO)

float_facile.cmxa : $(CSTRFCMX)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -a $(CSTRFCMX)

interval_byte : 
	$(MAKE) -C $(FILIBOCAML_DIR) all_byte

interval : 
	$(MAKE) -C $(FILIBOCAML_DIR) all

# Versions of the library to be linked with the -p (profiler) option
facile.p.cmxa : $(CSTR)
	rm -fr $(CSTRCMX)
	make facile.cmxa OPTOPT=-p
	rm -fr $(CSTRCMX)
	mv facile.cmxa $@
	mv facile.a facile.p.a

float_facile.p.cmxa : interval $(CSTRFCMX)
	rm -fr $(CSTRCMX)
	make facile.cmxa OPTOPT=-p
	make float_facile.cmxa OPTOPT=-p
	rm -fr $(CSTRFCMX) $(CSTRCMX)
	mv facile.cmxa facile.p.cmxa
	mv float_facile.cmxa $@
	mv float_facile.a float_facile.p.a
	mv facile.a facile.p.a

# Toplevel (to be run with the "-I +facile" option)
facile :  facile.cma
	$(OCAMLTOP) -custom -o $@ $(INCLUDES) facile.cma 

# Toplevel (to be run with the "-I +facile" option)
float_facile:  interval facile.cma float_facile.cma
	$(OCAMLTOP) -custom -o $@ $(FINCLUDES) facile.cma interval.cma float_facile.cma

# Experimental: to get inlining of functors using preprocessing:
# Does not work with many modules (e.g. containing class expressions)
facile.exp.cmxa : $(CSTR)
	rm -fr $(CSTRCMX)
	make fcl_stak.cmx fcl_cstr.cmx fcl_inv.cmx fcl_reify.cmx fcl_arith.cmx fcl_goals.cmx fcl_fdArray.cmx fcl_gcc.cmx fcl_alldiff.cmx fcl_sorting.cmx fcl_interval.cmx facile.cmx
	make facile.cmxa OPTOPT="-pp inline_functors"
	rm -fr $(CSTRCMX)
	mv facile.cmxa $@
	mv facile.a facile.exp.a

.SUFFIXES:
.SUFFIXES: .ml .mli .mly .mll .cmi .cmo .cmx .p.cmx .s .h .c .o

.ml.cmo :
	$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES) -c $<
.mli.cmi :
	$(OCAMLMLI) $(INCLUDES) -c $<
.ml.cmx :
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OPTOPT) $(INCLUDES_OPT) -c $<
.ml.s :
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OPTOPT) $(INCLUDES_OPT) -S -c $<
.mly.ml :
	$(OCAMLYACC) $<
.mll.ml :
	$(OCAMLLEX) $<
.c .o   :
	$(CC) -c $<

clean:
	\rm -f *.cmi *.cmo *.cmx *.o
#	make -C $(FILIBOCAML_DIR) clean

clean_all:
	\rm -f *.cm* *.annot *.o *.a .depend facile float_facile
#	make -C $(FILIBOCAML_DIR) clean_all

.depend:
	$(OCAMLDEP) *.mli *.ml > $@

include .depend
