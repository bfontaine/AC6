open AST
open Operator
open Runtime

(* The type for primitives. *)
type t 

(* [identifier x] returns true if [x] is a
   reserved identifier denoting a primitive. *)
val identifier : value_identifier -> bool

(* [lookup x] assuming that [x] is a primitive
   identifier, returns the corresponding primitive. *)
val lookup : value_identifier -> t value

(* [apply p v] assumes the [p] is a primitive function
   and apply this function on a value [v] returning 
   a new value. *)
val apply : t -> t value -> t value

(* This exception is raised if we tried to call a
   primitive that is not a function. *)
exception InvalidPrimitiveCall 

