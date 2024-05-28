type value = Yojson.Safe.t [@@deriving show]

let from_channel (ch : in_channel) : value =
  let line = In_channel.input_line ch in
  match line with Some x -> Yojson.Safe.from_string x | None -> `Null

let show (v : value) : string = Yojson.Safe.to_string v

let index_assoc (assoc : (string * value) list) (s : string) : value =
  try List.find (fun (str, _) -> str == s) assoc |> snd
  with Not_found -> `Null
