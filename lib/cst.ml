type query = Term of term | JoinedQuery of term * operator * query
[@@deriving show]

and term =
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

and operator = Pipe
