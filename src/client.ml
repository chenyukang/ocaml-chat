open Core
open Sys

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

let run_client addr port =
  try
    let sockaddr = Unix.ADDR_INET(addr, port) in
    let ic, oc = Unix.open_connection sockaddr
    in client_fun ic oc ;
    Unix.shutdown_connection ic
  with _ -> failwith "run_client failed ..."

