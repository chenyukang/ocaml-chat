OCAMLBUILD = cd ./src; corebuild -use-ocamlfind -pkg str,stringext,lwt,lwt.unix,logs,logs.lwt

defualt: chat

chat:
	$(OCAMLBUILD) chat.native

clean:
	cd ./src; rm -rf _build *.cmi *.o *.cmx *.cma

