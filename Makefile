OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

OCAMLC=ocamlfind ocamlc

OCAMLDEP=ocamlfind ocamldep

INCLUDES=

OCAMLFLAGS=$(INCLUDES)
OCAMLDEPFLAGS=$(INCLUDES)

BYTEFILES=size_change_termination.cmo  tools.cmo  parser.cmo lexer.cmo  main.cmo


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<


all: $(BYTEFILES)
	$(OCAMLC) $(INCLUDES) $(BYTEFILES) -o main main.ml

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
	rm -f main


include .depend
