let rec interpret' (last_value : Value.value) (program : Cst.program) :
    Value.value =
  match program with
  | Empty -> Null
  | Query query -> resolve_query last_value query

and resolve_query (last_value : Value.value) (query : Cst.query) : Value.value =
  match query with
  | Expr expr -> resolve_expr last_value expr
  | JoinedQuery (expr, query) ->
      let lh_factor = resolve_expr last_value expr in
      resolve_query lh_factor query

and resolve_expr (last_value : Value.value) (expr : Cst.expr) : Value.value =
  match expr with
  | Identity -> last_value
  | Literal literal -> literal
  | Grouping expr -> resolve_expr last_value expr
  | Binary (lh_expr, op, rh_expr) -> (
      let lh_number = resolve_expr last_value lh_expr |> Value.as_number in
      let rh_number = resolve_expr last_value rh_expr |> Value.as_number in
      match op with
      | Addition -> Number (lh_number + rh_number)
      | Subtraction -> Number (lh_number - rh_number)
      | Multiplication -> Number (lh_number * rh_number)
      | Division -> Number (lh_number / rh_number))

let interpret (program : Cst.program) = interpret' Value.Null program
