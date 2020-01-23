open Core
open Sys
open Lwt

let client_fun ic oc =
  try
    Util.print_cursor "Client";
    match Unix.fork () with
    | `In_the_parent _ -> (
      while true do
        match In_channel.input_line In_channel.stdin with
        | Some(str) -> (
            let msg = (Proto.msg_to_json_str str) ^ "\n" in
            Out_channel.output_string oc msg;
            Out_channel.flush oc;
          )
          | _ -> ()
      done
    )
    | `In_the_child -> (
        while true do
          match In_channel.input_line ic with
          | Some(r) -> (
              (* Printf.fprintf Out_channel.stdout "got: %s\n" r; *)
              let msg = Yojson.Basic.from_string r in
              match Proto.is_ack msg with
              | true -> Util.print_ack "Server" "Client" msg;
              | false -> (
                  Util.print_accepted_content "Server" "Client" msg;
                  let ack = (Proto.ack_str msg) ^ "\n" in
                  Out_channel.output_string oc ack;
                  Out_channel.flush oc;
              )
            )
          | _ -> ();
        done;
      )
  with
    Exit -> exit 0
  | exn -> Unix.shutdown_connection ic ; raise exn

let run_client addr port =
  try
    let sockaddr = Unix.ADDR_INET(addr, port) in
    let ic, oc = Unix.open_connection sockaddr
    in client_fun ic oc ;
    Unix.shutdown_connection ic
  with _ -> failwith "run_client failed ..."

