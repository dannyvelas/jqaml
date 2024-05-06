(*open Core*)

let rec repl lines =
  try
    let line = read_line () in
    repl (line :: lines)
  with End_of_file -> List.rev lines

let () =
  let lines = repl [] in
  List.iter (fun line -> print_endline line) lines
