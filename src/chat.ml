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

(* Shared mutable counter *)
let counter = ref 0
let run_mode = ref Unknown
let listen_address = ref UnixLabels.inet6_addr_loopback
let port = ref 4000
let backlog = 10

let handle_message msg =
  match msg with
  | "read" -> string_of_int !counter
  | "inc"  -> counter := !counter + 1; "Counter has been incremented"
  | _      -> "Unknown command"

let rec handle_connection ic oc () =
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
     match msg with
     | Some msg ->
       let reply = handle_message msg in
       Lwt_io.write_line oc reply >>= handle_connection ic oc
     | None -> Logs_lwt.info (fun m -> m "Connection closed") >>= return)

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc ()) (fun e -> Logs.err (fun m -> m "%s" (Caml.Printexc.to_string e) ));
  Logs_lwt.info (fun m -> m "New connection") >>= return

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = bind sock @@ ADDR_INET(!listen_address, !port) in
  listen sock backlog;
  sock

let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

let run_server () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let sock = create_socket () in
  let serve = create_server sock in
  Lwt_main.run @@ serve ()

let client_fun ic oc =
  try
    while true do
      print_string  "Send: ";
      Out_channel.flush Out_channel.stdout;
      let _ = match In_channel.input_line In_channel.stdin with
        | Some(str) -> (
          let str = str ^ "\n" in
          Out_channel.output_string oc str;
          Out_channel.flush oc;
        )
      | _ -> () in
      match In_channel.input_line ic with
      | Some(r) -> (
          Printf.fprintf Out_channel.stdout "Response: %s\n" r;
          if r = "END" then ( Unix.shutdown_connection ic ; raise Exit) ;
        )
      | _ -> ();
    done
  with
    Exit -> exit 0
  | exn -> Unix.shutdown_connection ic ; raise exn

let run_client () =
  try
    let sockaddr = Unix.ADDR_INET(!listen_address, !port) in
    let ic, oc = Unix.open_connection sockaddr
    in client_fun ic oc ;
    Unix.shutdown_connection ic
  with _ -> failwith "run_client failed ..."


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
    | Server -> run_server ()
    | Client -> run_client ()
    | _ -> failwith "error mode"
  end

let () = main
