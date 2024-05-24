exception TypeError of string

let as_number (factor : Cst.factor) : int =
  match factor with
  | True | False -> raise (TypeError "cannot perform arithmetic on boolean")
  | Identity -> raise (TypeError "did not expect identity factor")
  | Number number -> number
  | Null -> 0

let rec interpret (last_factor : Cst.factor) (query : Cst.query) : Cst.factor =
  match query with
  | Empty -> Null
  | Expr expr -> (
      match expr with
      | Term term -> (
          match term with Factor Identity -> last_factor | Factor x -> x)
      | Arithmetic (expr, op, term) -> (
          let lh_factor = Expr expr |> interpret last_factor in
          let rh_factor = Expr (Term term) |> interpret last_factor in
          let lh_number = as_number lh_factor in
          let rh_number = as_number rh_factor in
          match op with
          | Addition -> Number (lh_number + rh_number)
          | Subtraction -> Number (lh_number - rh_number)))
  | JoinedQuery (expr, query) -> (
      let lh_factor = Expr expr |> interpret last_factor in
      let rh_factor = interpret lh_factor query in
      match (lh_factor, rh_factor) with
      | lh_factor, Identity -> lh_factor
      | _, rh_factor ->
          (* if right hand factor is just a hard-coded value, return it *)
          rh_factor)
