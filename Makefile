# Make sure ocamlbuild can find opam-managed packages: first run
#
# eval `opam config env`

# Easiest way to build: using ocamlbuild, which in turn uses ocamlfind

clrClear = \033[0m
clrBlue  = \033[1;34m
clrGreen = \033[1;32m
clrRed   = \033[31;01m
clrYellow= \033[33;01m

default:
	@echo "\n$(clrGreen)--==[ $(clrBlue)Making sure that build is up to date$(clrGreen)]==--$(clrClear)"
	make all
	@echo "\n\n$(clrGreen)--==[ $(clrBlue)Printing out LLVM IR code for test.pm... $(clrGreen)]==--$(clrClear)"
	./pixmix.native test.pm
	@echo "\n\n$(clrGreen)--==[ $(clrBlue)Piping LLVM IR into interpreter... $(clrGreen)]==--$(clrClear)"
	./pixmix.native test.pm | lli $$1

.PHONY : all
all : 
	@eval `opam config env`
	make pixmix.native 

.PHONY : pixmix.native
pixmix.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 pixmix.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff pixmix scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe


# More detailed: build using ocamlc/ocamlopt + ocamlfind to locate LLVM
OBJS = ast.cmx codegen.cmx parser.cmx scanner.cmx semant.cmx pixmix.cmx

pixmix : $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o pixmix

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<

### Generated by "ocamldep *.ml *.mli" after building scanner.ml and parser.ml
ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo
codegen.cmx : ast.cmx
pixmix.cmo : semant.cmo scanner.cmo parser.cmi codegen.cmo ast.cmo
pixmix.cmx : semant.cmx scanner.cmx parser.cmx codegen.cmx ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
semant.cmo : ast.cmo
semant.cmx : ast.cmx
parser.cmi : ast.cmo
