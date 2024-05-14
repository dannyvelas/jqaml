{
open Parser
open Lexing

exception SyntaxError of string

let keyword_table = 
  let table = Hashtbl.create 3 in 
  Hashtbl.add table "null" NULL;
  Hashtbl.add table "true" TRUE;
  Hashtbl.add table "false" FALSE;
  table

(* stolen from: https://github.com/austral/austral/blob/a211109833fa5604c13f0f2eac86146bc2ed2639/lib/Lexer.mll#L49 *)
let advance_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  let pos' = { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  } in
  lexbuf.lex_curr_p <- pos'
}

(* Helper regexes *)

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = (alpha|digit)
let whitespace = [' ' '\t']+
let newline = "\r\n" | '\n'

(* Token regexes *)

let identifier = (alpha) ('_'|alphanum)*

(* Rules *)

rule token = parse 
| "." { PERIOD }
(* constants *)
| "=" { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| ".." { RECURSE }
| "." identifier { INDEX (Lexing.lexeme lexbuf) }
| identifier as word
  {
    try Hashtbl.find keyword_table word
    with Not_found -> IDENTIFIER (Lexing.lexeme lexbuf)
  }
(* whitespace and eof *)
| whitespace { token lexbuf } (* skip whitespace *)
| newline { advance_line lexbuf; token lexbuf }
| eof { EOF }
