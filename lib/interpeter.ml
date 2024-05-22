type interpret_result = Number of int | Null | Boolean of bool

let rec interpret_internal (query : Cst.query) : interpret_result =
  match query with
  | Cst.Empty -> Null
  | Cst.Term term -> (
      match term with
      | Null -> Null
      | True -> Boolean true
      | False -> Boolean false
      | Identity _ -> Null (* return input as is *)
      | Recurse -> Null (* recurse through input *)
      | Index _ -> Null (*index input*))
