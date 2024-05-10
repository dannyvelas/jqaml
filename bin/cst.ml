type program = Query of query [@@deriving show]
and query = Term of { term : term }
and term = Null | True | False | Identity | Recurse | Index of string
