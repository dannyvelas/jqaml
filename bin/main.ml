open Core

let get_next_token lexbuf =
  try Ok (Lexer.token lexbuf) with
  | Lexer.SyntaxError msg -> Error (Printf.sprintf "%s%!" msg)
  | Parser.Error ->
      Error
        (Printf.sprintf "At offset %d: syntax error.\n%!"
           (Lexing.lexeme_start lexbuf))

let token_to_str : Parser.token -> string = function
  | PIPE -> "PIPE"
  | PERIOD -> "PERIOD"
  | SEMI -> "SEMI"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | EQUAL -> "EQUAL"
  | DOLLAR -> "DOLLAR"
  | RECURSE -> "RECURSE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LCURLY -> "LCURLY"
  | RCURLY -> "RCURLY"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | INDEX s -> Printf.sprintf "INDEX %s\n" s
  | NULL -> "NULL"
  | NUMBER_CONSTANT d -> Printf.sprintf "NUMBER_CONSTANT %d\n" d
  | STRING_CONSTANT s -> Printf.sprintf "STRING_CONSTANT %s\n" s
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | DEF -> "DEF"
  | REDUCE -> "REDUCE"
  | FOREACH -> "FOREACH"
  | EOF -> "EOF"
  | IDENTIFIER s -> Printf.sprintf "IDENTIFIER %s\n" s

let run lexbuf =
  let rec repl () =
    let token = get_next_token lexbuf in
    match token with
    | Ok EOF -> ()
    | Ok x ->
        print_endline @@ token_to_str x;
        repl ()
    | Error msg -> print_endline msg
  in
  repl ()

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
  run @@ Lexing.from_channel inputch
