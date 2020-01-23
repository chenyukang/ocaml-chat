open Core

let print_cursor flag =
  Printf.fprintf Out_channel.stdout "[%s]: $=> " flag;
  Out_channel.flush Out_channel.stdout

let print_ack send recv msg =
  let snd = Proto.msg_send_tm msg in
  let ack = Proto.msg_ack_tm msg in
  let content = Proto.msg_content msg in
  Printf.fprintf Out_channel.stdout
    "{%s}: #=> ACK content(%s) [%f -> %f]\n" send content snd ack;
  print_cursor recv

let print_accepted_content send recv msg =
  let content = Proto.msg_content msg in
  Printf.fprintf Out_channel.stdout "\n{%s}: #=> %s\n" send content;
  print_cursor recv

