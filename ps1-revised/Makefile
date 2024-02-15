# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc

all: clean mips

mips:
	$(COMPILER) -c byte.ml
	$(COMPILER) -c format.ml
	$(COMPILER) -c mips_ast.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c mips_assem.ml
	$(COMPILER) -c mips_sim.ml
	$(COMPILER) -c mips.ml
	$(COMPILER) -o ps1 byte.cmo format.cmo mips_ast.cmo parse.cmo lex.cmo mips_assem.cmo mips_sim.cmo mips.cmo

clean:
	-rm *.cmo *.cmi ps1 parse.ml parse.mli lex.ml 
