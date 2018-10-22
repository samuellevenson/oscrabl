MODULES = board actions main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind


default:
	utop -init main.ml
	
build:
	ocamlbuild -use-ocamlfind main.cmo actions.cmo board.cmo test.cmo

test:
	ocamlbuild -use-ocamlfind -tag 'debug' test.byte && ./test.byte

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

check:
	bash checkenv.sh && bash checktypes.sh

docs:
	mkdir -p doc
	ocamldoc -d doc -html main.ml

clean:
	ocamlbuild -clean
	rm -rf doc
