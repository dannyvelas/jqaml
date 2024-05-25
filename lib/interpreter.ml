exception TypeError of string

let as_number (factor : Cst.factor) : int =
  match factor with
  | True | False -> raise (TypeError "cannot perform arithmetic on boolean")
  | Identity -> raise (TypeError "did not expect identity factor")
  | Number number -> number
  | Null -> 0

let rec interpret' (last_factor : Cst.factor) (query : Cst.query) : Cst.factor =
  match query with
  | Empty -> Null
  | Expr expr -> resolve_expr last_factor expr
  | JoinedQuery (expr, query) ->
      let lh_factor = resolve_expr last_factor expr in
      interpret' lh_factor query

and resolve_expr (last_factor : Cst.factor) (expr : Cst.expr) : Cst.factor =
  match expr with
  | Term term -> resolve_term last_factor term
  | Arithmetic (expr, op, term) -> (
      let lh_factor = resolve_expr last_factor expr |> as_number in
      let rh_factor = resolve_term last_factor term |> as_number in
      match op with
      | Addition -> Number (lh_factor + rh_factor)
      | Subtraction -> Number (lh_factor - rh_factor))

and resolve_term (last_factor : Cst.factor) (term : Cst.term) : Cst.factor =
  match term with Factor Identity -> last_factor | Factor x -> x

let interpret query = interpret' Cst.Null query
