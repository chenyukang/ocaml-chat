open Lwt
open Logs
open Core
open Lwt_unix
open Lwt_io

let rec handle_console oc () =
  Lwt_io.read_line_opt stdin >>=
  (function
    | Some(str) ->
      let str = Proto.msg_to_json_str str in
      (* Lwt_io.eprintf "%s" str >>= *)
      Lwt_io.write_line oc str >>=
      fun() -> Lwt_io.eprintf "[Server]: $=> " >>=
      handle_console oc
    | None ->
      Lwt_io.eprintf "input error...\n" >>= Lwt.return
  )

let rec handle_reply ic oc lwt_input () =
  Lwt_io.read_line_opt ic >>=
  (function
    | Some(str) -> (
        let msg = Yojson.Basic.from_string str in
        let rep = match Proto.is_ack msg with
          | true -> (
              Util.print_ack "Client" "Server" msg;
              Lwt_io.flush_all ()
            )
          | _ -> (
              Util.print_accepted_content "Client" "Server" msg;
              Proto.make_ack_str msg |> Lwt_io.write_line oc
            ) in
        rep >>= handle_reply ic oc lwt_input
      )
    | None -> (
        (* When client disconnect with server,
           we need to stop get input from Lwt_io.stdin.
           Otherwise, input maybe send to previous connection *)
        Lwt.cancel lwt_input;
        Lwt_io.eprintf "\nConnection Closed ...\n" >>=
        fun() -> Lwt_io.close oc >>=
        Lwt.return
    )
  )

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let fail_fun e = (Logs.debug (fun m -> m "%s" (Caml.Printexc.to_string e))) in
  let lwt_input = handle_console oc () in
  Lwt.on_failure lwt_input fail_fun;
  Lwt.on_failure (handle_reply ic oc lwt_input ()) fail_fun;
  Lwt_io.eprint "New Connection ...\n" >>=
  fun() -> Lwt_io.eprintf "[Server]: $=> " >>=
  fun() -> Lwt_io.flush_all () >>= Lwt.return

let create_socket listen_address port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ : unit Lwt.t = Lwt_unix.bind sock @@ ADDR_INET(listen_address, port) in
  listen sock 10;
  sock

let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

let run_server listen_address port =
  try
    let () = Logs.set_reporter (Logs.format_reporter ()) in
    let () = Logs.set_level (Some Logs.Info) in
    let sock = create_socket listen_address port in
    let serve = create_server sock in
    let _ : unit Lwt.t = Lwt_io.eprint "Wait client connect ...\n" in
    Lwt_main.run @@ serve ()
  with e ->
    Printf.fprintf Out_channel.stderr "%s\n" (Caml.Printexc.to_string e);
    exit(1)


