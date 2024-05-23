let rec interpret (query : Cst.query) : Cst.term =
  match query with
  | Cst.Empty -> Null
  | Cst.Term term -> term
  | Cst.JoinedQuery (term, _, query) -> (
      (* for now, operator is ignored bc it can only be PIPE *)
      let lh_term = term in
      let rh_term = interpret @@ query in
      match (lh_term, rh_term) with
      | lh_term, Identity _ -> lh_term
      | lh_term, Recurse ->
          lh_term (* make recurse behave like identity for now *)
      | lh_term, Index _ ->
          lh_term
          (* make Index behave like identitiy as objects are not implemented yet *)
      | lh_term, BracketSuffix _ ->
          lh_term
          (* make BracketSuffix behave like identity as strings/arrays are not implement yet *)
      | _, rh_term ->
          rh_term (* if right hand term is just a hard-coded value, return it *)
      )
