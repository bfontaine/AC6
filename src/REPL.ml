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

let start parse_input output e =
  print_banner ();
  let rec eval_loop ev =
    try
      let ast =
        parse_input (Lexing.from_string (read_entry ())) "(repl)" in
        let ev = Interpreter.eval ast ev in
          (* FIXME: List.hd on List.rev on List.rev_map is not efficient *)
          output (Runtime.print_env_identifier (List.hd (Runtime.Env.rev_entries ev)));
          print_newline ();
          eval_loop ev
    with
      End_of_file -> ()

    | Runtime.Env.UndeclaredVariable (AST.Identifier x) ->
        print_string ("Error: Undeclared variable '" ^ x ^ "'.\n");
        eval_loop ev

    | Runtime.Env.UndefinedVariable (AST.Identifier x) ->
        print_string ("Error: Undefined variable '" ^ x ^ "'.\n");
        eval_loop ev

    | Runtime.Env.DefiningUndeclaredVariable (AST.Identifier x) ->
        print_string ("Error: I can't define '" ^ x ^ "', it's undeclared.\n");
        eval_loop ev

    | Runtime.Env.CannotLookupAnonymous ->
        print_string "Error: Cannot lookup anonymous.\n";
        eval_loop ev

    | Runtime.Env.CannotDefineAnonymous ->
        print_string "Error: Cannot define anonymous.\n";
        eval_loop ev

    | Runtime.No_match ->
        print_string "Error: The pattern didn't match.\n";
        eval_loop ev

    | Primitive.InvalidPrimitiveCall ->
        print_string "Error: Invalid primitive call.\n";
        eval_loop ev
  in
    eval_loop e
