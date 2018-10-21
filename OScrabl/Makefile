default:
	utop -init main.ml
	
build:
	ocamlbuild -use-ocamlfind main.cmo test.cmo

test:
	ocamlbuild -use-ocamlfind -tag 'debug' test.byte && ./test.byte

check:
	bash checkenv.sh && bash checktypes.sh

docs:
	mkdir -p doc
	ocamldoc -d doc -html main.ml

clean:
	ocamlbuild -clean
	rm -rf doc
