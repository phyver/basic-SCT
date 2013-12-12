OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

OCAMLC=ocamlfind ocamlc

OCAMLOPT=ocamlfind ocamlopt

OCAMLDEP=ocamlfind ocamldep

INCLUDES=

OCAMLFLAGS=$(INCLUDES)
OCAMLDEPFLAGS=$(INCLUDES)

BYTEFILES=size_change_termination.cmo  tools.cmo  parser.cmo lexer.cmo
OPTFILES=size_change_termination.cmx  tools.cmx  parser.cmx lexer.cmx


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
		$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

all: $(BYTEFILES)
	$(OCAMLC) $(INCLUDES) $(BYTEFILES) -o sct main.ml

opt: $(OPTFILES)
	$(OCAMLOPT) $(INCLUDES) $(OPTFILES) -o sct.opt main.ml

dep: parser.ml lexer.ml
	$(OCAMLDEP) $(OCAMLDEPFLAGS) *.ml *.mli > .depend

parser.ml:
	$(OCAMLYACC) $(OCAMLYACCFLAGS) parser.mly

lexer.ml:
	$(OCAMLLEX) $(OCAMLLEXFLAGS) lexer.mll

clean:
	rm -f *.mli
	rm -f *.cm[oix] *.o
	rm -f parser.ml parser.mli
	rm -f lexer.ml
	rm -f sct sct.opt


include .depend
