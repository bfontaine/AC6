(** This module implemented an experimental REPL for Clap. *)

(**
 * Eval loop. [start parse output ev] print a primpt, use [parse] to parse an
 * input, evaluate it using the environment [ev], and print the new environment
 * with [output], and loop.
 **)
val start : (Lexing.lexbuf -> string -> AST.program) -> (Pprint.document -> unit) -> Primitive.t Runtime.venv -> unit
