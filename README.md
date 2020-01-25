
## one-on-one server-client chat Application in OCaml



## How to compile 

```shell
opam swith 4.09.0
eval `opam config env`
opam install -y core lwt logs stringext yojson merlin

make 
```

## Usage 

```shell
./src/chat.native -m server -p 4001  # start server 
./src/chat.native -m client -p 4001  # start client
# Ctrl-C will exit the applicaton
```

## Misc

* Use *lwt* in server
* Use fork in client to process *stdin.input*


Only tested on OCaml compiler verion: *4.09.0*.

