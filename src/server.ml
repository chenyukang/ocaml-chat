open Lwt
open Logs
open Core

let backlog = 10

let rec handle_connection_reply ic oc () =
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
     match msg with
     | Some(str) -> (
       let msg = Yojson.Basic.from_string str in
       match Proto.is_ack msg with
       | false -> (
           Util.print_accepted_content "Client" "Sever" msg;
           let reply = Proto.ack_str msg in
           Lwt_io.write_line oc reply >>=
           handle_connection_reply ic oc
         )
       | true -> (
           Util.print_ack "Client" "Server" msg;
           Lwt_io.flush_all () >>= handle_connection_reply ic oc
         )
     )
     | None ->
       Logs_lwt.info (fun m -> m "Connection closed\n[Server] #=> ") >>= Lwt.return
  )

let rec handle_connection_out oc () =
  (* print_cursor (); *)
  match In_channel.input_line In_channel.stdin with
  | Some(str) ->
    let str = (Proto.msg_to_json_str str) in
    let _ = Lwt_io.write_line oc str in
    Lwt_io.flush_all () >>=
    handle_connection_out oc
  | None -> Logs_lwt.info (fun m -> m "No input from server" >>= Lwt.return)

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let log = Logs_lwt.info (fun m -> m "New connection") in
  Util.print_cursor "Server";
  let _ = match Unix.fork () with
    | `In_the_parent _ ->
        Lwt.on_failure (handle_connection_out oc ())
          (fun e -> Logs.err (fun m -> m "%s" (Caml.Printexc.to_string e)))
    | `In_the_child -> (
        Lwt.on_failure (handle_connection_reply ic oc ())
          (fun e -> Logs.err (fun m -> m "%s" (Caml.Printexc.to_string e)))) in
  log >>= Lwt.return

let create_socket listen_address port =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = Lwt_unix.bind sock @@ ADDR_INET(listen_address, port) in
  listen sock backlog;
  sock

let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

let run_server listen_address port =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let sock = create_socket listen_address port in
  let serve = create_server sock in
  Lwt_main.run @@ serve ()
