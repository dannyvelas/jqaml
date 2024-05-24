type query = Empty | Expr of expr | JoinedQuery of expr * operator * query
[@@deriving show]

and expr = Term of term | Arithmetic of expr * operator * term
and term = Factor of factor

and factor =
  | Null
  | True
  | False
  | Identity of bracket_suffix list
  | Recurse
  | Index of string
  | BracketSuffix of bracket_suffix
  | Number of int

and bracket_suffix = Iteration | BracketIndex of index

and index =
  | BracketQuery of int
  | StartSlice of int
  | EndSlice of int
  | StartEndSlice of int * int

and operator = Pipe | Addition
