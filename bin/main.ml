open Core

let option_default default opt = match opt with Some x -> x | None -> default

let () =
  In_channel.input_line In_channel.stdin
  |> option_default "nothing" |> printf "%s\n"
