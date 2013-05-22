
let print_prompt () =
  (print_string "clap> ")

(**
 * print a prompt and read an entry from the user. It keeps reading until the
 * user press <enter> on an empty line.
 **)
let read_entry () =
  print_prompt ();
  let rec read_lines previous =
    let s = read_line () in
      if s = ""
      then previous
      else read_lines (previous ^ " " ^ s)
  in
    read_lines ""
