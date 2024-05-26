type program = Empty | Query of query [@@deriving show]
and query = Expr of expr | JoinedQuery of expr * query

and expr =
  | Binary of expr * operator * expr
  | Literal of literal
  | Identity
  | Grouping of expr

and literal = Null | True | False | Number of int
and operator = Addition | Subtraction | Multiplication | Division

let show_literal (literal : literal) : string =
  match literal with
  | Null -> "null"
  | True -> "true"
  | False -> "false"
  | Number number -> Printf.sprintf "%d" number
