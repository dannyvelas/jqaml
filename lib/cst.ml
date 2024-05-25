type query = Empty | Expr of expr | JoinedQuery of expr * query
[@@deriving show]

and expr = Term of term | ExprArithmetic of expr * add_or_subtract * term
and term = Factor of factor | TermArithmetic of term * mult_or_divide * factor
and factor = Identity | Literal of literal | ParenExpr of expr
and literal = Null | True | False | Number of int
and add_or_subtract = Addition | Subtraction
and mult_or_divide = Multiplication | Division
