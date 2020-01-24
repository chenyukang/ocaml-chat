open Yojson
open Yojson.Basic.Util

type 't msg_ty = [
  | `Assoc of (string * 't)
  | `Bool of bool      (* is ACK? *)
  | `Int of int        (* send time *)
  | `String of string  (* content *)
]

let is_ack m = m |> member "is_ack" |> to_bool
let msg_send_tm m = m |> member "send_time" |> to_float
let msg_content m = m |> member "content" |> to_string

let make_msg content =
  `Assoc
    [
      ("is_ack", `Bool false);
      ("content", `String content);
      ("send_time", `Float (Unix.time ())) ]


let make_ack m =
  `Assoc
    [
      ("is_ack", `Bool true);
      ("content", `String (m |> member "content" |> to_string));
      ("send_time", `Float (m |> member "send_time" |> to_float)) ]

let create_ack_str m =
  Yojson.Basic.to_string (make_ack m)

let create_msg_json_str c =
  make_msg c |> Yojson.Basic.to_string
