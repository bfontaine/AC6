let repl_version = "0.1.0"

let print_prompt () =
  (print_string "clap> ")

let print_banner () =
  print_string ("** Clap REPL v" ^ repl_version ^ "\n");
  print_string "**\n";
  print_string "** Press ^D to exit.\n";
  print_string "**\n"

let read_entry () =
  print_prompt ();
  let rec read_lines previous =
    let s = read_line () in
      if s = ""
      then previous
      else read_lines (previous ^ " " ^ s)
  in
    read_lines ""
