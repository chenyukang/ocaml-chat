open Core
open Sys

let run_client () =
  try
    let sockaddr = Unix.ADDR_INET(!listen_address, !port) in
    let ic, oc = Unix.open_connection sockaddr
    in client_fun ic oc ;
    Unix.shutdown_connection ic
  with Failure("int_of_string") -> failwith "bad port number"
