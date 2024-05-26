exception TypeError of string

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
      let lh_value = resolve_expr last_value lh_expr in
      let rh_value = resolve_expr last_value rh_expr in
      match (lh_value, op, rh_value) with
      | Number lh_number, Addition, Number rh_number ->
          Number (lh_number + rh_number)
      | Number lh_number, Subtraction, Number rh_number ->
          Number (lh_number - rh_number)
      | Number lh_number, Multiplication, Number rh_number ->
          Number (lh_number * rh_number)
      | Number lh_number, Division, Number rh_number ->
          Number (lh_number / rh_number)
      | String lh_string, Addition, String rh_string ->
          String (lh_string ^ rh_string)
      | _ -> raise (TypeError "cannot perform arithmetic on boolean"))

let interpret (program : Cst.program) : Value.value =
  interpret' Value.Null program
