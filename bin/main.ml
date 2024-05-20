open Jqaml_core

let usage_msg = "jqaml (-f <file> | <jq filter>)"

let bad_arg_msg =
  "Bad argument: jq src can be a command line string argument, or a file, but \
   not both"

let input_file = ref ""
let jq_src_arg = ref ""

let speclist =
  [
    ( "-f",
      Arg.Set_string input_file,
      "read from file. if not present, will read from stdin" );
  ]

let () =
  let set_jq_src_arg jq_src_arg' = jq_src_arg := jq_src_arg' in
  Arg.parse speclist set_jq_src_arg usage_msg;
  let jq_src =
    match (!jq_src_arg, !input_file) with
    | jq_src, "" -> jq_src
    | "", file -> In_channel.with_open_bin file In_channel.input_all
    | _, _ -> raise (Arg.Bad bad_arg_msg)
  in
  match Compiler.compile jq_src with
  | Error msg -> print_endline msg
  | Ok (Some query) -> print_endline @@ Cst.show_query query
  | Ok None -> ()
