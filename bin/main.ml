open Core

let run inputch =
  let lexbuf = Lexing.from_channel inputch in
  try
    let repl () =
      let tokenStr =
        match Lexer.token lexbuf with
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
      in
      print_endline tokenStr
    in
    repl ();
    repl ()
  with
  | Lexer.SyntaxError msg -> Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start lexbuf);
      In_channel.close inputch

(*
let load x = match x with
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
  | INDEX -> "INDEX"
  | NULL -> "NULL"
  | NUMBER -> "NUMBER" _CONSTANT
  | STRING -> "STRING" _CONSTANT
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | DEF -> "DEF"
  | REDUCE -> "REDUCE"
  | FOREACH -> "FOREACH"
  | EOF -> "EOF"
  *)

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
  run inputch
