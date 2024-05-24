let ( let* ) = Result.bind

let factor_to_int (factor : Cst.factor) : (int, string) result =
  match factor with
  | Cst.Null -> Ok 0
  | Cst.Number number -> Ok number
  | Cst.True | Cst.False -> Error "cannot perform arithmetic on boolean"
  | _ -> Error "unimplemented"

let rec interpret (query : Cst.query) : (Cst.factor, string) result =
  match query with
  | Cst.Empty -> Ok Cst.Null
  | Cst.Expr expr -> (
      match expr with
      | Cst.Term term -> ( match term with Factor factor -> Ok factor)
      | Cst.Arithmetic (expr, _, term) ->
          let* lh_factor = Cst.Expr expr |> interpret in
          let* rh_factor = Cst.Expr (Cst.Term term) |> interpret in
          let lh_factor', rh_factor' =
            (factor_to_int lh_factor, factor_to_int rh_factor)
          in
          Ok Cst.Null)
  | Cst.JoinedQuery (expr, _, query) -> (
      let lh_factor = interpret @@ Cst.Expr expr in
      let rh_factor = interpret query in
      match (lh_factor, rh_factor) with
      | Error err, _ -> Error err
      | _, Error err -> Error err
      (* pipe operator *)
      | lh_factor, Ok (Cst.Identity _) -> lh_factor
      | lh_factor, Ok Cst.Recurse ->
          (* make recurse behave like identity for now *)
          lh_factor
      | lh_factor, Ok (Cst.Index _) ->
          (* make Index behave like identitiy as objects are not implemented yet *)
          lh_factor
      | lh_factor, Ok (Cst.BracketSuffix _) ->
          (* make BracketSuffix behave like identity as strings/arrays are not implement yet *)
          lh_factor
      | _, rh_factor ->
          (* if right hand factor is just a hard-coded value, return it *)
          rh_factor)
