{
open Parser

exception SyntaxError of string
}

(* Helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = (alpha|digit)

(* Token regexes *)

let identifier = (alpha) ('_'|alphanum)*

(* Rules *)

rule token = parse 
| "." { PERIOD }
| "null" { NULL }
| "true" { TRUE }
| "false" { FALSE }
| ".." { RECURSE }
| "." identifier { INDEX (Lexing.lexeme lexbuf) }
