open Core

let () =
  let s = In_channel.input_line In_channel.stdin in
  match s with None -> printf "none" | Some x -> printf "%s\n" x
