(** Multi-client server example.

    Application should start in two modes:
    - as a server, waiting for one client to connect or
    - as a client, taking an IP address (or hostname) of server to connect to.
*)

open Core
open Sys
open Lwt
open Logs


type mode =
  | Server
  | Client
  | Unknown


let run_mode = ref Unknown
let listen_address = ref UnixLabels.inet6_addr_loopback
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

let set_port p =
  port := p

let main =
  begin
    let speclist = [("-m", Arg.String (set_mode), "Run in server/client mode");
                    ("-a", Arg.String (set_addr), "Set bind host addr");
                    ("-p", Arg.Int (set_port), "Set port no");
                   ]
    in let usage_msg = "Chat: OCaml chat server/client"
    in Arg.parse speclist print_endline usage_msg;
    match !run_mode with
    | Server -> Server.run_server !listen_address !port
    | Client -> Client.run_client !listen_address !port
    | _ -> failwith "error mode"
  end

let () = main
