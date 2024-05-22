open Jqaml_core

let usage_msg = "jqaml (-f <file> | <jq filter>)"
let input_file = ref ""
let jq_src_arg = ref ""

let bad_arg_msg =
  "Bad argument: jq src can be a command line string argument, or a file, but \
   not both"

let speclist =
  [
    ( "-f",
      Arg.Set_string input_file,
      "read from file. if not present, will read from stdin" );
  ]

let () =
  (* parse command line arguments *)
  let set_jq_src_arg jq_src_arg' = jq_src_arg := jq_src_arg' in
  Arg.parse speclist set_jq_src_arg usage_msg;

  (* read jq_src from one of two possible command line arguments *)
  let jq_src =
    match (!jq_src_arg, !input_file) with
    | jq_src, "" -> jq_src
    | "", file -> In_channel.with_open_bin file In_channel.input_all
    | _, _ -> raise (Arg.Bad bad_arg_msg)
  in

  (* compile jq_src and print it out as a CST *)
  match Compiler.compile jq_src with
  | Error msg -> print_endline msg
  | Ok query -> print_endline @@ Cst.show_query query
