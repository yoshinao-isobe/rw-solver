#  Copyright (C) 2018
#  AIST Program-IP No. H30PRO-2219
#  National Institute of Advanced Industrial Science and Technology (AIST),
#  East Japan Railway Company (JR-East)

SOURCES = \
	model.ml \
	getopt.ml \
	solver.ml \
	utils.ml \
	rsexpr.ml \
	ungetch.ml \
	loader.ml \
	viz.ml \
	viz2.ml \
	main.ml

ifdef OCAML_TOPLEVEL_PATH
CAIRO_LIB_PATH=$(OCAML_TOPLEVEL_PATH)/../cairo
else
CAIRO_LIB_PATH=+cairo
endif

# ifndef OSTYPE
#   OSTYPE = $(shell uname -o|awk '{print tolower($$0)}')
# endif
#
# ifeq ($(OSTYPE), cygwin)
# EXE=.exe
# else
# EXE=
# endif

OCAMLC_OPTIONS = -bin-annot
LIBS = str.cma cairo.cma unix.cma
LIBXS = str.cmxa cairo.cmxa unix.cmxa

%.cmi %.cmo:	%.ml
	ocamlc -c $(OCAMLC_OPTIONS) -I $(CAIRO_LIB_PATH) -I ./ $*.ml

%.cmi %.cmx:	%.ml
	ocamlopt -c $(OCAMLC_OPTIONS) -I $(CAIRO_LIB_PATH) -I ./ $*.ml

all: rwsolver

rwsolver:	getopt.cmo model.cmo parser.cmo lexer.cmo utils.cmo ungetch.cmo rsexpr.cmo \
			loader.cmo solver.cmo viz.cmo viz2.cmo main.cmo
	ocamlc -custom -o $@ -I $(CAIRO_LIB_PATH) $(LIBS) $+ 

rwsolverx:	getopt.cmx model.cmx parser.cmx lexer.cmx utils.cmx ungetch.cmx rsexpr.cmx \
			loader.cmx solver.cmx viz.cmx viz2.cmx main.cmx
	ocamlopt -o $@ -I $(CAIRO_LIB_PATH) $(LIBXS) $+ 

parser.mli parser.ml:	parser.mly model.cmo
	ocamlyacc -v parser.mly

parser.cmi: parser.mli parser.ml
	ocamlc parser.mli

parser.cmo: parser.cmi parser.ml
	ocamlc -c parser.ml

lexer.cmi lexer.cmo:	lexer.mll parser.cmi
	ocamllex lexer.mll
	ocamlc -c lexer.ml

clean:
	rm *.mli *.cmi *.cmo *.cmx *.cmt *.o *.exe *.svg parser.ml lexer.ml parser.output \
		rwsolver rwsolverx

dep:
	ocamldep $(SOURCES) >> Makefile


model.cmo :
model.cmx :
getopt.cmo :
getopt.cmx :
solver.cmo : utils.cmo ungetch.cmo rsexpr.cmo model.cmo getopt.cmo
solver.cmx : utils.cmx ungetch.cmx rsexpr.cmx model.cmx getopt.cmx
utils.cmo :
utils.cmx :
rsexpr.cmo : utils.cmo ungetch.cmo
rsexpr.cmx : utils.cmx ungetch.cmx
ungetch.cmo : utils.cmo
ungetch.cmx : utils.cmx
loader.cmo : parser.cmi model.cmo lexer.cmo
loader.cmx : parser.cmx model.cmx lexer.cmx
viz.cmo : utils.cmo solver.cmo model.cmo
viz.cmx : utils.cmx solver.cmx model.cmx
viz2.cmo : viz.cmo utils.cmo solver.cmo model.cmo getopt.cmo
viz2.cmx : viz.cmx utils.cmx solver.cmx model.cmx getopt.cmx
main.cmo : viz2.cmo viz.cmo utils.cmo solver.cmo model.cmo loader.cmo \
    getopt.cmo
main.cmx : viz2.cmx viz.cmx utils.cmx solver.cmx model.cmx loader.cmx \
    getopt.cmx
