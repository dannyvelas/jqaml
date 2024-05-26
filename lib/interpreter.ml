exception TypeError of string

let binary_err = "cannot use binary operator on a non-number or string type"
let unary_err = "cannot use unary operator on a non-number type"

let rec interpret (program : Cst.program) : Value.value =
  interpret' Value.Null program

and interpret' (last_value : Value.value) (program : Cst.program) : Value.value
    =
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
      let lh_value = resolve_expr last_value lh_expr in
      let rh_value = resolve_expr last_value rh_expr in
      match (lh_value, op, rh_value) with
      | Number lh, Addition, Number rh -> Number (lh + rh)
      | Number lh, Subtraction, Number rh -> Number (lh - rh)
      | Number lh, Multiplication, Number rh -> Number (lh * rh)
      | Number lh, Division, Number rh -> Number (lh / rh)
      | String lh, Addition, String rh -> String (lh ^ rh)
      | _ -> raise (TypeError binary_err))
  | Unary (operator, operand_expr) -> (
      let operand_value = resolve_expr last_value operand_expr in
      match (operator, operand_value) with
      | Positive, Number number -> Number number
      | Negative, Number number -> Number ~-number
      | _ -> raise (TypeError unary_err))
