open Core

let repl inputch =
  let lexbuf = Lexing.from_channel inputch in
  try
    let program = Parser.prog Lexer.token lexbuf in
    print_endline (Cst.show_program program)
  with
  | Lexer.SyntaxError msg -> Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start lexbuf);
      In_channel.close inputch

let () =
  let argv = Sys.get_argv () in
  let filename =
    match argv.(1) with exception Invalid_argument _ -> None | x -> Some x
  in
  let inputch =
    match filename with
    | None -> In_channel.stdin
    | Some filename -> In_channel.create filename
  in
  repl inputch
