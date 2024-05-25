type query = Empty | Expr of expr | JoinedQuery of expr * query
[@@deriving show]

and expr = Term of term | Arithmetic of expr * operator * term
and term = Factor of factor
and factor = Identity | Literal of literal
and literal = Null | True | False | Number of int
and operator = Addition | Subtraction | Multiplication | Division
