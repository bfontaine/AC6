(* When the following flag is set, a type checking algorithm
   stops if the input program is not well-typed. *)
val flag : bool ref

(* [program p] ensures that the evaluation of [p] does not go
   wrong. *)
val program : AST.program -> unit
