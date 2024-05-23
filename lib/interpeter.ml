type interpret_result = Number of int | Null | Boolean of bool | Identity | Index

let rec interpret_internal (query : Cst.query): interpret_result =
  match query with
  | Cst.Empty -> Null
  | Cst.Term term -> (
      match term with
      | Null -> Null
      | True -> Boolean true
      | False -> Boolean false
      | Identity _ -> Identity
      | Recurse -> Identity
          (* recurse through input: not implemented yet bc objects are not in grammar  *)
      | Index _ ->
          Index
      | Number num -> Number num)
    | Cst.JoinedQuery term op query -> (
      let term_result = interpret_internal @@ Cst.query term in
      let query_result = interpret_internal @@ query in

  )
