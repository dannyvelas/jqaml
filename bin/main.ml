open Core

let () =
  let inx = In_channel.create "hi.calc" in
  let lexbuf = Lexing.from_channel inx in
  try
    let a = Parser.main Lexer.token lexbuf in
    List.iter ~f:(fun a -> printf "%d\n" a) a
  with
  | Lexer.SyntaxError msg -> Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start lexbuf);
      In_channel.close inx
