type program = Empty | Query of query [@@deriving show]
and query = Expr of expr | JoinedQuery of expr * query

and expr =
  | Binary of expr * operator * expr
  | Literal of Value.value
  | Identity
  | Grouping of expr
  | Unary of unaryop * expr

and operator =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | GreaterThan
  | LessThan
  | GreaterThanEqual
  | LessThanEqual
  | Equal

and unaryop = Positive | Negative
