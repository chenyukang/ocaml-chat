open Lwt
open Logs
open Core
open Lwt_unix

let rec handle_connection_reply ic oc () =
  Lwt_io.read_line_opt ic >>=
  (function
    | Some(str) -> (
        let msg = Yojson.Basic.from_string str in
        if Proto.is_ack msg then (
         Util.print_ack "Client" "Server" msg;
         Lwt_io.flush_all () >>= handle_connection_reply ic oc
       ) else (
         Util.print_accepted_content "Client" "Server" msg;
         let ack = Proto.make_ack_str msg in
         Lwt_io.write_line oc ack >>=
         handle_connection_reply ic oc
         ))
    | None ->
      let _ = Lwt_io.close oc >>= Lwt.return in
      Logs_lwt.info (fun m -> m "\nConnection closed...\n") >>= Lwt.return
  )

let rec handle_connection_out oc () =
  Util.print_cursor "Server";
  Lwt_io.read_line_opt Lwt_io.stdin >>=
  (function
    | Some(str) ->
      let str = (Proto.msg_to_json_str str) in
      Lwt_io.write_line oc str
      >>= handle_connection_out oc
    | None ->
      Logs_lwt.info (fun m -> m "\nNo input...\n") >>= Lwt.return
  )

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let log = Logs_lwt.info (fun m -> m "New Connection ...") in
  let fail_fun e = (Logs.debug (fun m -> m "%s" (Caml.Printexc.to_string e))) in
  Lwt.on_failure (handle_connection_out oc ()) fail_fun;
  Lwt.on_failure (handle_connection_reply ic oc ()) fail_fun;
  log >>= Lwt.return

let create_socket listen_address port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = Lwt_unix.bind sock @@ ADDR_INET(listen_address, port) in
  listen sock 10;
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
