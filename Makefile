INSTALLDIR=$(HOME)/.local/bin
OCAMLBUILD=ocamlbuild

all: native

tags:
	ctags *.ml

native:
	$(OCAMLBUILD) main.native
	@ln -sf ./main.native ./sct

byte:
	$(OCAMLBUILD) main.byte
	@ln -sf ./main.byte ./sct

clean:
	$(OCAMLBUILD) -clean
	rm -rf _build
	rm -f main.native main.byte
	rm -f sct
	rm -f tags
	rm -f gmon.out a.out



