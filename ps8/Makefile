# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc

all: clean cish cfg

mips:
	$(COMPILER) -c word32.ml
	$(COMPILER) -c mips.ml

cish: mips
	$(COMPILER) -c cish_ast.ml
	ocamlyacc cish_parse.mly
	$(COMPILER) -c cish_parse.mli
	$(COMPILER) -c cish_parse.ml
	ocamllex cish_lex.mll
	$(COMPILER) -c cish_lex.ml
	$(COMPILER) -c cish_eval.ml
	$(COMPILER) -c cish_compile.ml

cfg: mips cish
	$(COMPILER) -c cish_ast.ml
	$(COMPILER) -c cfg_ast.ml
	$(COMPILER) -c spill.ml
	$(COMPILER) -c graph.ml
	$(COMPILER) -c cfg.ml
	$(COMPILER) -c regalloc.ml
	$(COMPILER) -c cfg_compile.ml
	$(COMPILER) -c main.ml
	$(COMPILER) -o ps8_cfg cish_ast.cmo cish_lex.cmo cish_parse.cmo word32.cmo mips.cmo cfg_ast.cmo graph.cmo spill.cmo cfg.cmo regalloc.cmo cfg_compile.cmo main.cmo

clean:
	-rm *.cmo *.cmi ps8_cfg cish_parse.ml cish_parse.mli cish_lex.ml 
