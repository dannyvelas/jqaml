open Core

let () =
  let lines = In_channel.input_lines In_channel.stdin in
  List.iter lines ~f:(fun line -> print_endline line)
