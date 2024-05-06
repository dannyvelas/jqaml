open Core

let () =
  In_channel.iter_lines In_channel.stdin ~f:(fun l ->
      printf "%d\n" (String.length l))
