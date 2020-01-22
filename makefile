OCAMLBUILD = cd ./src; corebuild -use-ocamlfind -pkg str,stringext,lwt,lwt.unix,logs,logs.lwt

defualt: chat

prepare:
	opam install -y core lwt logs stringext

chat:
	$(OCAMLBUILD) chat.native

clean:
	cd ./src; rm -rf _build *.cmi *.o *.cmx *.cma *.native

