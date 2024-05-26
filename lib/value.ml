type value = Null | True | False | Number of int | String of string

let show (value : value) : string =
  match value with
  | Null -> "null"
  | True -> "true"
  | False -> "false"
  | Number number -> string_of_int number
  | String s -> s
