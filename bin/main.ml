open Core

let rec repl () =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | Some x ->
      print_endline x;
      repl ()
  | None -> ()

let () = repl ()
