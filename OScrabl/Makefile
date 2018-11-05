MODULES = board actions moment main words ai
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,ANSITerminal,Yojson

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)


check:
	bash checkenv.sh && bash checktypes.sh

docs:	docs-private

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		$(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc

zip:
	zip src.zip *.ml* *.json _tags Makefile
