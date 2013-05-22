
let pretty_print = ref false
let parse_only = ref false
let benchmark = ref false
let repl = ref false

let options = Arg.align [
  "-p", Arg.Set pretty_print, 
  " Display the parsed input on stdout.";

  "--parse-only", Arg.Set parse_only, 
  " Do syntax analysis only.";

  "--bench", Arg.Set benchmark, 
  " Benchmark.";

  "--memo", Arg.Set Memo.flag, 
  " Memoization";

  "--type", Arg.Set Typecheck.flag, 
  " Typecheck before evaluation.";

  (* experimental *)
  "--repl", Arg.Set repl,
  "Use a REPL instead of the provided files."
]

(* helper function *)
let parse_input lex filename =
  let parser lexer lexbuf = try
    Parser.program lexer lexbuf
  with
  | Parser.Error -> Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  SyntacticAnalysis.process
    ~lexer_init: (fun _ -> lex)
    ~lexer_fun: Lexer.top
    ~parser_fun: parser
    ~input: filename

let parse_file filename =
  parse_input (Lexing.from_channel (open_in filename)) filename

let filenames = ref []

let usage = "usage: clap [options] input1 ... inputN"

let _ = Arg.parse options (fun s -> filenames := s :: !filenames) usage

let output = Pprint.Channel.pretty 0.8 80 stdout 

let bench f = 
  let start = Unix.gettimeofday () in
  let y = f () in
  let stop = Unix.gettimeofday () in 
  (stop -. start, y)

let process_file filename = 
  let ast = parse_file filename in 
  if !pretty_print then 
    output (Printer.program ast);
  if not !parse_only then (
    Typecheck.program ast;
    let time, result = bench (fun () -> Interpreter.program ast) in
    if !benchmark then 
      output (Pprint.text (Printf.sprintf "TIME[%07.3fs %s]\n" time filename));
    output (Runtime.print_environment result)
  )

(* REPL support *)
let asts =
  if !repl
  then
    (REPL.print_banner ();
    let rec eval_loop ev =
      try
        let ast =
          parse_input (Lexing.from_string (REPL.read_entry ())) "(repl)" in
          let ev = Interpreter.eval ast ev in
            output (Runtime.print_environment ev);
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
      
      | Primitive.InvalidPrimitiveCall ->
          print_string "Error: Invalid primitive call.\n";
          eval_loop ev
    in
      eval_loop (Runtime.Env.empty ()); [])
  else
(* /REPL support *)
    List.map process_file !filenames
