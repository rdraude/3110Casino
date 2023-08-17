.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch


test:
	OCAMLRUNPARAM=b dune exec roulette/test/main.exe
 	OCAMLRUNPARAM=b dune exec blackjack/test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe


doc:
	dune build @doc

clean:
	dune clean

zip:
	rm -f final.zip
	zip -r final.zip . 
