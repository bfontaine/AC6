let repl_version = "0.2.0"

(* This exception is used as a passed message to clear the environment *)
exception Clear_env

(* Primary prompt *)
let ps1 () =
  (print_string "clap> ")

(* Secondary prompt *)
let ps2 () =
  (print_string "   ?> ")

(* Prints a string followed by a newline *)
let pl s =
  print_string (s ^ "\n")

(* Prints a banner with the REPL version *)
let print_banner () =
  pl ("** Clap REPL v" ^ repl_version);
  pl "**";
  pl "** Press ^D to exit.";
  pl "**"

(* Prints a basic help *)
let print_help () =
  pl "";
  pl "-- Help --";
  pl "";
  pl "/clear           : clear the environment";
  pl "/exit, /quit, ^D : exit the REPL";
  pl "/help            : print this help";
  pl ""

(* Read an entry from the user *)
let rec read_entry () =
  ps1 ();
  let rec read_lines previous =
    let s = read_line () in
      match s with
      | "/exit"  -> raise Exit
      | "/quit"  -> raise Exit
      | "/clear" -> raise Clear_env
      | "/help"  -> (print_help (); read_entry ())

      | "" -> previous
      | s  ->
        (ps2 (); read_lines (previous ^ " " ^ s))
  in
    read_lines ""

(* Starts the REPL *)
let start parse_input output e =
  print_banner ();
  let rec eval_loop ev =
    try
      let ast =
        parse_input (Lexing.from_string (read_entry ())) "(repl)" in
        let ev = Interpreter.eval ast ev in
          output (Runtime.print_env_identifier (Runtime.Env.last_entry ev));
          print_newline ();
          eval_loop ev
    with
      End_of_file -> ()
    | Exit        -> ()

    | Clear_env -> eval_loop (Runtime.Env.empty ())

    | Not_found -> eval_loop ev

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
