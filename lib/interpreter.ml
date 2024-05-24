let ( let* ) = Result.bind

exception TypeError of string

let as_number (factor : Cst.factor) : int =
  match factor with
  | Cst.True | Cst.False ->
      raise (TypeError "cannot perform arithmetic on boolean")
  | Cst.Identity -> raise (TypeError "did not expect identity factor")
  | Cst.Number number -> number
  | Cst.Null -> 0

let rec interpret (query : Cst.query) : Cst.factor =
  match query with
  | Cst.Empty -> Cst.Null
  | Cst.Expr expr -> (
      match expr with
      | Cst.Term term -> ( match term with Factor factor -> factor)
      | Cst.Arithmetic (expr, op, term) -> (
          let lh_factor = Cst.Expr expr |> interpret in
          let rh_factor = Cst.Expr (Cst.Term term) |> interpret in
          let lh_number = as_number lh_factor in
          let rh_number = as_number rh_factor in
          match op with
          | Cst.Addition -> Cst.Number (lh_number + rh_number)
          | Cst.Subtraction -> Cst.Number (lh_number - rh_number)))
  | Cst.JoinedQuery (expr, query) -> (
      let lh_factor = interpret @@ Cst.Expr expr in
      let rh_factor = interpret query in
      match (lh_factor, rh_factor) with
      | lh_factor, Cst.Identity -> lh_factor
      | _, rh_factor ->
          (* if right hand factor is just a hard-coded value, return it *)
          rh_factor)
