let parse_next_token lexbuf =
  try Ok (Parser.prog Lexer.token lexbuf) with
  | Lexer.SyntaxError msg -> Error (Printf.sprintf "%s%!" msg)
  | Parser.Error ->
      Error
        (Printf.sprintf "At offset %d: syntax error.\n%!"
           (Lexing.lexeme_start lexbuf))

let run_parser lexbuf =
  let rec repl () =
    let cst = parse_next_token lexbuf in
    match cst with
    | Ok x ->
        print_endline @@ Cst.show_program x;
        repl ()
    | Error msg -> print_endline msg
  in
  repl ()

(*
let get_next_token lexbuf =
  try Ok (Lexer.token lexbuf)
  with Lexer.SyntaxError msg -> Error (Printf.sprintf "%s%!" msg)

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
  | INDEX s -> Printf.sprintf "INDEX %s" s
  | NULL -> "NULL"
  | NUMBER_CONSTANT d -> Printf.sprintf "NUMBER_CONSTANT %d" d
  | STRING_CONSTANT s -> Printf.sprintf "STRING_CONSTANT %s" s
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | DEF -> "DEF"
  | REDUCE -> "REDUCE"
  | FOREACH -> "FOREACH"
  | EOF -> "EOF"
  | IDENTIFIER s -> Printf.sprintf "IDENTIFIER %s" s
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | ASSIGN -> "ASSIGN"
  | EOL -> "EOL"

let run_lexer lexbuf =
  let rec repl () =
    let token = get_next_token lexbuf in
    match token with
    | Ok token ->
        print_endline @@ token_to_str token;
        if token != EOF then
          repl ()
    | Error msg -> print_endline msg
  in
  repl ()
  *)

let () =
  let inputch =
    match Sys.argv.(1) with
    | exception Invalid_argument _ -> In_channel.stdin
    | file -> open_in file
  in
  run_parser @@ Lexing.from_channel inputch
