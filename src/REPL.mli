(** This module implemented an experimental REPL for Clap. *)

(** print a banner with some info about the REPL *)
val print_banner : unit -> unit

(**
 * print a prompt and read an entry from the user. It keeps reading until the
 * user press <enter> on an empty line.
 **)
val read_entry : unit -> string

(**
 * Eval loop. [start parse output ev] print a primpt, use [parse] to parse an
 * input, evaluate it using the environment [ev], and print the new environment
 * with [output], and loop.
 **)
val start : (Lexing.lexbuf -> string -> AST.program) -> (Pprint.document -> unit) -> Primitive.t Runtime.venv -> unit
