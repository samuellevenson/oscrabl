MODULES = board actions moment main words
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind


build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

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

zip:
	zip src.zip *.ml* *.json _tags Makefile
