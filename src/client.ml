open Core
open Sys
open Lwt

let client_fun ic oc =
  try
    let print_cursor () =
      print_string  "[Client] #=> ";
      Out_channel.flush Out_channel.stdout in
    match Unix.fork () with
    | `In_the_parent pid -> (
      while true do
        print_cursor ();
        match In_channel.input_line In_channel.stdin with
        | Some(str) -> (
            let msg = (Proto.msg_to_json_str str) ^ "\n" in
            (* Printf.fprintf Out_channel.stdout "msg: %s" msg; *)
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
              Printf.fprintf Out_channel.stdout "got: %s\n" r;
              let m = Yojson.Basic.from_string r in
              match Proto.is_ack m with
              | true -> (
                  let send = Proto.msg_send_tm m in
                  let ack = Proto.msg_ack_tm m in
                  Printf.fprintf Out_channel.stdout "ack: [%f -> %f]\n" send ack;
                )
              | false ->
                Printf.fprintf Out_channel.stdout "Response: %s\n" r;
              print_cursor ();
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

