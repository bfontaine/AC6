(** This module defines the type of Abstract Syntax Trees. *)

(** A program is a list of definitions. *)
type program = definitions

and definitions = definition list

(** Definitions introduce type identifiers and value
    identifiers. Each identifier is associated to a definition
    depending on its kind. *)
and definition = 
| DType of type_identifier * type_identifiers * typ
| DVal  of vdefinition

(** Definitions of values are splitted into two categories: *)
and vdefinition =
(** Simple value definition. *)
| Simple  of binding * expr
(** Mutually recursive definitions (of functions). *)
| MutuallyRecursive of (binding * expr) list

and binding = Binding of argument_identifier * typ option

and argument_identifier = 
| Named of value_identifier
| Unnamed

(** The syntax for types. *)
and typ = 
  | TVar   of type_identifier * types (** Type constructor application. *)
  | TArrow of typ * typ               (** Function types.               *)
  | TSum   of constructor_types       (** Sum types.                    *)
  | TProd  of constructor_types       (** Product types.                *)
  | TRec   of type_identifier * typ   (** Recursive types.              *)

and types = typ list

and constructor_type = TConstructor of constructor_identifier * typ option

and constructor_types = constructor_type list

(** The syntax for expressions. *)
and expr = 
  | EInt	 of int
  | EChar	 of char
  | EString      of string
  | EVar	 of value_identifier
  | ESum	 of constructor_identifier * typ option * expr option
  | EProd	 of typ option * constructor_definitions
  | EAnnot	 of expr * typ
  | ESeq	 of exprs
  | EDef   	 of vdefinition * expr
  | EApp         of expr * expr
  | ECase	 of typ option * branches
  | EFun         of binding * expr 

and exprs = expr list

and constructor_definition = constructor_identifier * expr option

and constructor_definitions = constructor_definition list

and branch = Branch of (pattern * expr)

and branches = branch list

and pattern = 
  | PSum  of constructor_identifier * typ option * pattern option
  | PProd of typ option * (constructor_identifier * pattern option) list
  | PAnd  of pattern * pattern
  | POr   of pattern * pattern
  | PNot  of pattern
  | PZero 
  | PVar  of value_identifier
  | POne 

and value_identifier = Identifier of string

and constructor_identifier = CIdentifier of string

and type_identifier = TIdentifier of string * int

and type_identifiers = type_identifier list
