open Core

let tokenize _s = []

let rec repl () =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | Some x ->
      let _tokens = tokenize x in
      repl ()
  | None -> ()

let () = repl ()
