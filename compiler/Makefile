OCAMLMAKEFILE=../OCamlMakefile

SOURCES=util.mli location.mli config.mli config.ml ident.mli graph.mli \
	asttypes.mli util.ml location.ml ast.mli ident.ml graph.ml asttypes.ml \
	types.mli parser.mly ast.ml patternmatch.mli env.mli dependency.mli \
	types.ml lexer.mll codegen.ml typing.mli patternmatch.ml \
	env.ml dependency.ml typing.ml fpc.ml

LIBS=unix
RESULT=fp

.PHONY: all
all: nc
	@ :

TRASH=lexer.ml parser.mli parser.ml

include $(OCAMLMAKEFILE)
