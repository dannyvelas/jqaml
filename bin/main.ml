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
  (new_state, next_byte)

let is_at_end state = state.pos >= String.length state.input
let prepend_token state token = { state with tokens = token :: state.tokens }

type err = { message : string; value : char }

let update_state state byte =
  match byte with
  | '.' ->
      Ok (prepend_token state { tokenType = DOT; lexeme = Char.to_string byte })
  | 'a' .. 'z' | 'A' .. 'Z' -> Ok state
  | '0' .. '9' -> Ok state
  | '|' -> Ok state
  | '"' -> Ok state
  | '[' -> Ok state
  | ']' -> Ok state
  | _ -> Error { message = "did not recognize byte"; value = byte }

let tokenize s =
  let rec tokenize state =
    if is_at_end state then
      (* state.tokens is stored in reverse for performance reasons (cons is constant time, append is linear)
         so we have to reverse the list before returning *)
      Ok (List.rev state.tokens)
    else
      let state = { state with start = state.pos } in
      let state, byte = next state in
      let stateResult = update_state state byte in
      stateResult |> Result.bind ~f:tokenize
  in
  tokenize { input = s; start = 0; pos = 0; tokens = [] }

let rec repl () =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | Some x ->
      let _tokens = tokenize x in
      repl ()
  | None -> ()

let () = repl ()
