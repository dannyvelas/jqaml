(*open Core*)

let () =
  try
    while true do
      let line = read_line () in
      print_endline line
    done
  with End_of_file -> ()
