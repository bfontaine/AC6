
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

  "--debug", Arg.Set Debug.flag,
  " Debug mode.";

  "--memo", Arg.Set Memo.flag, 
  " Memoization";

  "--type", Arg.Set Typecheck.flag, 
  " Typecheck before evaluation.";

  "--repl", Arg.Set repl,
  " Use a REPL instead of the provided files."
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

let asts =
  if not !repl
  then
    List.map process_file !filenames
  else
    (REPL.start parse_input (output) (Runtime.Env.empty ()); [])
