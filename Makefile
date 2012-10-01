bin=/usr/local/bin

ack :
	ocamlbuild main.native -libs unix,str

install :
	mkdir -p $(bin)
	cp main.native $(bin)/ackdo

uninstall :
	rm -f $(bin)/ackdo

clean : 
	ocamlbuild -clean
