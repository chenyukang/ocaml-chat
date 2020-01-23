open Yojson
open Yojson.Basic.Util

type msg_ty = [
  | `Assoc of (string * json)
  | `Bool of bool      (* is ACK? *)
  | `Int of int        (* send time *)
  | `Int of int        (* ACK  time *)
  | `String of string  (* content *)
]

let is_ack m =
  m |> member "is_ack" |> to_bool

let msg_ack_tm m =
  m |> member "ack_time" |> to_float

let msg_send_tm m =
  m |> member "send_time" |> to_float

let make_msg content =
  `Assoc
    [
      ("is_ack", `Bool false);
      ("content", `String content);
      ("ack_time", `Float 0.);
      ("send_time", `Float (Unix.time ())) ]


let make_ack m =
  `Assoc
    [
      ("is_ack", `Bool true);
      ("content", `String (m |> member "content" |> to_string));
      ("ack_time", `Float (Unix.time ()));
      ("send_time", `Float (m |> member "send_time" |> to_float)) ]


let msg_to_json_str m =
  let json = make_msg m in
  Yojson.Basic.to_string json
