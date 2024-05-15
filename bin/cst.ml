type program = Expr of expr
and expr = Number of int | Term of int * operator * int [@@deriving show]
and operator = MINUS | PLUS | MULT
