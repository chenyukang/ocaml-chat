open Core

let print_cursor flag =
  Printf.fprintf Out_channel.stderr "[%s]: $=> " flag;
  Out_channel.flush Out_channel.stderr

let print_ack send recv msg =
  let snd_tm = Proto.msg_send_tm msg in
  let content = Proto.msg_content msg in
  let ack_tm = Unix.time () in
  Printf.fprintf Out_channel.stderr
    "{%s}: #=> ACK content: [%s] timestamp: [%.2f -> %.2f = %.2f]\n"
    send content snd_tm ack_tm (ack_tm -. snd_tm);
  print_cursor recv

let print_accepted_content send recv msg =
  let content = Proto.msg_content msg in
  Printf.fprintf Out_channel.stderr "{%s}: #=> %s\n" send content;
  print_cursor recv

