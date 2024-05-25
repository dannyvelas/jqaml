let parse_next_token lexbuf =
  try Ok (Parser.prog Lexer.token lexbuf) with
  | Lexer.SyntaxError msg -> Error (Printf.sprintf "%s%!" msg)
  | Parser.Error ->
      Error
        (Printf.sprintf "At offset %d: syntax error.\n%!"
           (Lexing.lexeme_start lexbuf))

let compile (jq_src : string) : (Cst.literal, string) result =
  let lexbuf = Lexing.from_string jq_src in
  let cst = parse_next_token lexbuf in
  let json = Result.map Cst.yojson_of_query cst in
  (match json with
  | Ok json -> print_endline @@ Yojson.Safe.to_string json
  | Error msg -> print_endline msg);
  let value = Result.map Interpreter.interpret cst in
  value
