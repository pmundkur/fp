.PHONY: all clean copyright

all:
	ocamlbuild -use-ocamlfind all.otarget

clean:
	ocamlbuild -clean

copyright:
	headache -c mk/headache.conf -h mk/copyright.headache runtime/*.ml compiler/*.mli compiler/*.ml compiler/*.mll compiler/*.mly doc/*.tex doc/*.ott
