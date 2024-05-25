exception TypeError of string

let as_number (literal : Cst.literal) : int =
  match literal with
  | True | False -> raise (TypeError "cannot perform arithmetic on boolean")
  | Number number -> number
  | Null -> 0

let rec interpret' (last_literal : Cst.literal) (query : Cst.query) :
    Cst.literal =
  match query with
  | Empty -> Null
  | Expr expr -> resolve_expr last_literal expr
  | JoinedQuery (expr, query) ->
      let lh_factor = resolve_expr last_literal expr in
      interpret' lh_factor query

and resolve_expr (last_literal : Cst.literal) (expr : Cst.expr) : Cst.literal =
  match expr with
  | Term term -> resolve_term last_literal term
  | ExprArithmetic (expr, op, term) -> (
      let lh_factor = resolve_expr last_literal expr |> as_number in
      let rh_factor = resolve_term last_literal term |> as_number in
      match op with
      | Addition -> Number (lh_factor + rh_factor)
      | Subtraction -> Number (lh_factor - rh_factor))

and resolve_term (last_literal : Cst.literal) (term : Cst.term) : Cst.literal =
  match term with
  | Factor factor -> resolve_factor last_literal factor
  | TermArithmetic (term, op, factor) -> (
      let lh_factor = resolve_term last_literal term |> as_number in
      let rh_factor = resolve_factor last_literal factor |> as_number in
      match op with
      | Multiplication -> Number (lh_factor * rh_factor)
      | Division -> Number (lh_factor / rh_factor))

and resolve_factor (last_literal : Cst.literal) (factor : Cst.factor) :
    Cst.literal =
  match factor with Identity -> last_literal | Literal literal -> literal

let interpret query = interpret' Cst.Null query
