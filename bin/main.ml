open Core

type tokenType =
  | DOT
  | LEFTSQ
  | RIGHTSQ
  | NUMBER of int
  | IDENTIFIER of string
  | PIPE
  | DOUBLEQUOTE
  | DEF

type token = { tokenType : tokenType; lexeme : string }
type state = { input : string; start : int; pos : int; tokens : token list }

let peek state = state.input.[state.pos]

let next state =
  let next_byte = peek state in
  let new_state = { state with pos = state.pos + 1 } in
  (next_byte, new_state)

let is_at_end state = state.pos >= String.length state.input
let append_token state token = { state with tokens = state.tokens @ token }

let update_state state byte =
  match byte with '.' -> append_token state { tokenType = DOT; lexeme = byte }

let tokenize s ?(state = { input = s; start = 0; pos = 0; tokens = [] }) =
  if is_at_end state then
    state.tokens
  else
    let state = { state with start = state.pos } in
    let state, byte = next state in
    tokenize (update_state state byte)

let rec repl () =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | Some x ->
      let _tokens = tokenize x in
      repl ()
  | None -> ()

let () = repl ()
