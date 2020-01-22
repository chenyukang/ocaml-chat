open Core
open Sys
open Lwt

let client_fun ic oc =
  try
    match Unix.fork () with
    | `In_the_parent pid -> (
      while true do
        print_string  "#=> ";
        Out_channel.flush Out_channel.stdout;
        match In_channel.input_line In_channel.stdin with
          | Some(str) -> (
              let str = str ^ "\n" in
              Out_channel.output_string oc str;
              Out_channel.flush oc;
            )
          | _ -> ()
      done
    )
    | `In_the_child -> (
        while true do
          match In_channel.input_line ic with
          | Some(r) -> (
              Printf.fprintf Out_channel.stdout "Response: %s\n" r;
              print_string  "#=> ";
              Out_channel.flush Out_channel.stdout;
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

