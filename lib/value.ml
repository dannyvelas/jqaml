exception TypeError of string
exception InternalError of string

type value = Null | True | False | Number of int | String of string

let show (value : value) : string =
  match value with
  | Null -> "null"
  | True -> "true"
  | False -> "false"
  | Number number -> string_of_int number
  | String s -> s

let as_number (value : value) : int =
  match value with
  | True | False -> raise (TypeError "cannot perform arithmetic on boolean")
  | String _ ->
      raise (InternalError "received a string where a number was expected")
  | Number number -> number
  | Null -> 0
