compile:
	ocamlbuild -use-ocamlfind board.byte gui.byte main.byte
	ocamlbuild -use-ocamlfind AI.byte

server:
	ocamlbuild -use-ocamlfind server.byte
	./server.byte 12344

white:
	./gui.byte White 127.0.0.1 12344 a

black:
	./gui.byte Black 127.0.0.1 12344 a

main:
	./main.byte

clean:
	ocamlbuild -clean

test:
	ocamlbuild -use-ocamlfind test.byte test_ai.byte
	./test.byte
	./test_ai.byte

wai:
	ocamlbuild -use-ocamlfind AI.byte gui.byte
	./gui.byte White 127.0.0.1 12344 ai

bai: 
	ocamlbuild -use-ocamlfind AI.byte gui.byte
	./gui.byte Black 127.0.0.1 12344 ai

