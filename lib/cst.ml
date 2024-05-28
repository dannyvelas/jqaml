type program = Empty | Query of query
and query = Expr of expr | JoinedQuery of expr * query

and expr =
  | Binary of expr * operator * expr
  | Literal of Value.value
  | Identity
  | Grouping of query
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
  | NotEqual

and unaryop = Positive | Negative
