type value = Null | Bool of bool | Number of int | String of string
[@@deriving show]

let show (value : value) : string =
  match value with
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Number number -> string_of_int number
  | String s -> s
