(** This module implemented an experimental REPL for Clap. *)

(** print a banner with some info about the REPL *)
val print_banner : unit -> unit

(**
 * print a prompt and read an entry from the user. It keeps reading until the
 * user press <enter> on an empty line.
 **)
val read_entry : unit -> string

