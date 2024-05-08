(*open Core*)

let repl inputch =
  let lexbuf = Lexing.from_channel inputch in
  try
    let a = Parser.main Lexer.token lexbuf in
    Core.List.iter ~f:(fun a -> Core.printf "%d\n" a) a
  with
  | Lexer.SyntaxError msg -> Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start lexbuf);
      In_channel.close inputch

let () =
  let argv = Core.Sys.get_argv () in
  let filename =
    match argv.(1) with exception Invalid_argument _ -> None | x -> Some x
  in
  let inputch =
    Option.fold ~none:In_channel.stdin
      ~some:(fun f -> Core.In_channel.create f)
      filename
  in
  repl inputch
