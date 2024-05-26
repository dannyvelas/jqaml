exception TypeError of string

let as_number (literal : Cst.literal) : int =
  match literal with
  | True | False -> raise (TypeError "cannot perform arithmetic on boolean")
  | Number number -> number
  | Null -> 0

let rec interpret' (last_literal : Cst.literal) (program : Cst.program) :
    Cst.literal =
  match program with
  | Empty -> Null
  | Query query -> resolve_query last_literal query

and resolve_query (last_literal : Cst.literal) (query : Cst.query) : Cst.literal
    =
  match query with
  | Expr expr -> resolve_expr last_literal expr
  | JoinedQuery (expr, query) ->
      let lh_factor = resolve_expr last_literal expr in
      resolve_query lh_factor query

and resolve_expr (last_literal : Cst.literal) (expr : Cst.expr) : Cst.literal =
  match expr with
  | Identity -> last_literal
  | Literal literal -> literal
  | Grouping expr -> resolve_expr last_literal expr
  | Binary (lh_expr, op, rh_expr) -> (
      let lh_number = resolve_expr last_literal lh_expr |> as_number in
      let rh_number = resolve_expr last_literal rh_expr |> as_number in
      match op with
      | Addition -> Number (lh_number + rh_number)
      | Subtraction -> Number (lh_number - rh_number)
      | Multiplication -> Number (lh_number * rh_number)
      | Division -> Number (lh_number / rh_number))

let interpret (program : Cst.program) = interpret' Cst.Null program
