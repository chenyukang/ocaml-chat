(**
    one-on-one chat Application in OCaml

    Application should start in two modes:
    - as a server, waiting for one client to connect or
    - as a client, taking an IP address (or hostname) of server to connect to.
*)

open Core
open Sys

type mode =
  | Server
  | Client

let run_mode = ref Server
let listen_address = ref UnixLabels.inet_addr_loopback
let port = ref 4000

let set_mode mode =
  match mode with
  | "s" | "server" -> run_mode := Server
  | "c" | "client" -> run_mode := Client
  | _ -> failwith "Invalid mode"

let set_addr addr =
  listen_address :=
    try UnixLabels.inet_addr_of_string addr
    with _ ->
      failwith "Unknown server ..."

let set_port p = port := p

let main =
  begin
    let speclist =
      [
        ("-m", Arg.String (set_mode), "Run in server/client mode (default server)");
        ("-a", Arg.String (set_addr), "Set bind host addr (default 127.0.0.1)");
        ("-p", Arg.Int (set_port), "Set port number (default 4000)");
      ] in
    let usage_msg = "Chat: OCaml chat server/client" in
    Arg.parse speclist print_endline usage_msg;
    match !run_mode with
    | Server -> Server.run_server !listen_address !port
    | Client -> Client.run_client !listen_address !port
  end

let () = main
