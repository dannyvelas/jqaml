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

(* re-use same buffer for strings. remember to clear after using *)
let string_buffer: Buffer.t = Buffer.create 64
}

(* Helper regexes *)

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = (alpha|digit)
let whitespace = [' ' '\t']+
let newline = "\r\n" | '\n'

(* Token regexes *)

let identifier = (alpha) ('_'|alphanum)*
let dec_constant = digit+

(* Rules *)

rule token = parse 
  | "." { PERIOD }
  (* symbols *)
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | ":" { COLON }
  | "=" { ASSIGN }
  | "==" { EQ }
  | "!=" { NEQ }
  | ".." { RECURSE }
  | "+" { PLUS }
  (* constants *)
  | dec_constant { NUMBER_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) } 
  (* idents *)
  | "." identifier as s { INDEX (String.sub s 1 (String.length s - 1)) }
  | identifier as word
    {
      try Hashtbl.find keyword_table word
      with Not_found -> IDENTIFIER (Lexing.lexeme lexbuf)
    }
  | '"' { read_string lexbuf }
  (* whitespace and eof *)
  | whitespace { token lexbuf } (* skip whitespace *)
  | newline { advance_line lexbuf; token lexbuf }
  | eof { EOF }

and read_string = parse
  | '"' { let c = Buffer.contents string_buffer in Buffer.clear string_buffer; STRING_CONSTANT c }
  | [^ '"'] { Buffer.add_string string_buffer (Lexing.lexeme lexbuf); read_string lexbuf }
