open Core

let () =
  let filename = "data" in
  let max_number =
    let open In_channel in
    with_file ~binary:false filename
      ~f:(fold_lines ~init:0 ~f:(fun m s -> Int.(max m @@ of_string s)))
  in
  printf "Max number is %s is %d\n" filename max_number
