let parse_next_token lexbuf =
  try Ok (Parser.prog Lexer.token lexbuf) with
  | Lexer.SyntaxError msg -> Error (Printf.sprintf "%s%!" msg)
  | Parser.Error ->
      Error
        (Printf.sprintf "At offset %d: syntax error.\n%!"
           (Lexing.lexeme_start lexbuf))

let run_parser lexbuf =
  let rec repl () =
    print_string "new program: ";
    let cst = parse_next_token lexbuf in
    match cst with
    | Ok x ->
        print_endline @@ Cst.show_program x;
        if x <> Cst.Query None then
          repl ()
    | Error msg -> print_endline msg
  in
  repl ()

let get_next_token lexbuf =
  try Ok (Lexer.token lexbuf)
  with Lexer.SyntaxError msg -> Error (Printf.sprintf "%s%!" msg)

let token_to_str = function
  | Parser.PIPE -> "PIPE"
  | Parser.PERIOD -> "PERIOD"
  | Parser.SEMI -> "SEMI"
  | Parser.COMMA -> "COMMA"
  | Parser.COLON -> "COLON"
  | Parser.EQUAL -> "EQUAL"
  | Parser.DOLLAR -> "DOLLAR"
  | Parser.RECURSE -> "RECURSE"
  | Parser.LBRACKET -> "LBRACKET"
  | Parser.RBRACKET -> "RBRACKET"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.LCURLY -> "LCURLY"
  | Parser.RCURLY -> "RCURLY"
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.MUL -> "MUL"
  | Parser.DIV -> "DIV"
  | Parser.AND -> "AND"
  | Parser.OR -> "OR"
  | Parser.NOT -> "NOT"
  | Parser.INDEX s -> Printf.sprintf "INDEX %s" s
  | Parser.NULL -> "NULL"
  | Parser.NUMBER_CONSTANT d -> Printf.sprintf "NUMBER_CONSTANT %d" d
  | Parser.STRING_CONSTANT s -> Printf.sprintf "STRING_CONSTANT %s" s
  | Parser.TRUE -> "TRUE"
  | Parser.FALSE -> "FALSE"
  | Parser.DEF -> "DEF"
  | Parser.REDUCE -> "REDUCE"
  | Parser.FOREACH -> "FOREACH"
  | Parser.EOF -> "EOF"
  | Parser.IDENTIFIER s -> Printf.sprintf "IDENTIFIER %s" s
  | Parser.EQ -> "EQ"
  | Parser.NEQ -> "NEQ"
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.EOL -> "EOL"

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

let usage_msg = "jqaml [-p] [-f <output>]"
let input_file = ref ""
let parse = ref false

let speclist =
  [
    ( "-p",
      Arg.Set parse,
      "if this argument is present, the program will output the parse tree \
       instead of a token stream" );
    ( "-f",
      Arg.Set_string input_file,
      "read from file. if not present, will read from stdin" );
  ]

let () =
  let raise_bad_argument x = raise (Arg.Bad ("Bad argument : " ^ x)) in
  Arg.parse speclist raise_bad_argument usage_msg;
  let inputch =
    match !input_file with "" -> In_channel.stdin | _ -> open_in !input_file
  in
  if !parse then
    run_parser @@ Lexing.from_channel inputch
  else
    run_lexer @@ Lexing.from_channel inputch
