(* Programs are evaluated into environments containing values whose
   primitives are implemented by the module {!Primitive}. *)
type value = Primitive.t Runtime.value
type env = Primitive.t Runtime.venv

(* [program p] returns the final evaluation environment 
   obtained by evaluating the program [p]. *)
val program : AST.program -> env
