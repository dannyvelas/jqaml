let ( let* ) = Result.bind

exception TypeError of string

let check_type (factor : Cst.factor) =
  match factor with
  | Cst.True | Cst.False ->
      raise (TypeError "cannot perform arithmetic on boolean")
  | _ -> ()

let rec interpret (query : Cst.query) : Cst.factor =
  match query with
  | Cst.Empty -> Cst.Null
  | Cst.Expr expr -> (
      match expr with
      | Cst.Term term -> ( match term with Factor factor -> factor)
      | Cst.Arithmetic (expr, _, term) ->
          let lh_factor = Cst.Expr expr |> interpret in
          let rh_factor = Cst.Expr (Cst.Term term) |> interpret in
          check_type lh_factor;
          check_type rh_factor;
          Cst.Null)
  | Cst.JoinedQuery (expr, _, query) -> (
      let lh_factor = interpret @@ Cst.Expr expr in
      let rh_factor = interpret query in
      match (lh_factor, rh_factor) with
      (* pipe operator *)
      | lh_factor, Cst.Identity _ -> lh_factor
      | lh_factor, Cst.Recurse ->
          (* make recurse behave like identity for now *)
          lh_factor
      | lh_factor, Cst.Index _ ->
          (* make Index behave like identitiy as objects are not implemented yet *)
          lh_factor
      | lh_factor, Cst.BracketSuffix _ ->
          (* make BracketSuffix behave like identity as strings/arrays are not implement yet *)
          lh_factor
      | _, rh_factor ->
          (* if right hand factor is just a hard-coded value, return it *)
          rh_factor)
