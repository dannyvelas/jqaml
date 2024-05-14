type program = Query of query [@@deriving show]
and query = Term of { term : term }

and term =
  | Null
  | True
  | False
  | Identity of bracket_suffix list
  | Recurse
  | Index of string
  | BracketSuffix of bracket_suffix
  | Number of int

and bracket_suffix = Iteration | Index of index

and index =
  | BracketQuery of query
  | StartSlice of query
  | EndSlice of query
  | StartEndSlice of query * query
