(** This module implements a pretty-printer for {!AST.program} *)

(** [program p] transforms [p] as a document whose printed version 
    is a valid input for {!Parser.program}. *)
val program: AST.program -> Pprint.document
