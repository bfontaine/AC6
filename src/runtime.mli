open AST

exception No_match (* raised when a pattern-matching doesn't match *)

module Env : sig

  (* The type of environment associating a value of type ['a]
     to an identifier. *)
  type 'a t

  (* [bind x v e] returns an environment where [x] is 
     associated to [v] and the remaining entries are 
     the ones of [e]. *)
  val bind    : argument_identifier -> 'a -> 'a t -> 'a t

  (* [declare x e] introduces [x] in the environment [e]
     without any associated value. *)
  val declare : argument_identifier -> 'a t -> 'a t

  (* [define x v e] assumes that [x] is already declared 
     in [e] but with no associated value. This function
     associates [v] to this identifier [x] by modifiying
     [e] in place. *)
  val define  : argument_identifier -> 'a -> 'a t -> unit

  (* [empty ()] is the empty environment. *)
  val empty   : unit -> 'a t

  (* [lookup x e] returns the value associated to [x] in [e]. *)
  val lookup  : argument_identifier -> 'a t -> 'a

  (* [entries e] returns all the pair formed with identifier
     and their associated values in [e]. *)
  val entries : 'a t -> (value_identifier * 'a) list

  (* [last_entry e] returns the last entry. *)
  val last_entry : 'a t -> (value_identifier * 'a)

  exception DefiningUndeclaredVariable of value_identifier
  exception UndefinedVariable of value_identifier
  exception UndeclaredVariable of value_identifier
  exception CannotDefineAnonymous
  exception CannotLookupAnonymous
end

(* The syntax for values parameterized by the type 
   of the primitives. *)
type 'p value = 
| VInt       of int
| VChar      of char
| VString    of string
| VStruct    of ('p value option) structure
| VClosure   of 'p venv * branches
| VPrimitive of 'p

and 'p structure = (constructor_identifier * 'p) list

(* The environments stored inside closures associates a value
   to every identifier. In other words, they are evaluation
   environments. *)
and 'p venv = 'p value Env.t

(* The unit value. *)
val vunit : 'p value

(* [print v] outputs a value as a document. *)
val print : 'p value -> Pprint.document

(* [print_env_identifier i v] outputs an environment binding
   as a document *)
val print_env_identifier : AST.value_identifier * 'p value -> Pprint.document

(* [print v] outputs an environment as a document. *)
val print_environment : 'p venv -> Pprint.document

(* [canonicalize s] returns a structure whose compononents
   are sorted by alphabetical order of their constructor names. *)
val canonicalize : 'a structure -> 'a structure


  
