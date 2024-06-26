let parse_next_token lexbuf =
  try Ok (Parser.prog Lexer.token lexbuf) with
  | Lexer.SyntaxError msg -> Error (Printf.sprintf "%s%!" msg)
  | Parser.Error ->
      Error
        (Printf.sprintf "At offset %d: syntax error.\n%!"
           (Lexing.lexeme_start lexbuf))

let compile (jq_src : string) : (Value.value, string) result =
  let lexbuf = Lexing.from_string jq_src in
  let cst = parse_next_token lexbuf in
  let input_value = Value.from_channel In_channel.stdin in
  (*(match cst with
    | Ok cst -> print_endline @@ Cst.show_query cst
    | Error _ -> ());*)
  let value = Result.map (Interpreter.interpret input_value) cst in
  value
