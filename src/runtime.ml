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

  (* [rev_entries e] is the reverse of [entries e]. *)
  val rev_entries : 'a t -> (value_identifier * 'a) list

  exception DefiningUndeclaredVariable of value_identifier
  exception UndefinedVariable of value_identifier
  exception UndeclaredVariable of value_identifier
  exception CannotDefineAnonymous
  exception CannotLookupAnonymous

end = struct

  type 'a t = (value_identifier * 'a option ref) list

  let entries env = try List.rev_map (fun (x, r) ->
    match !r with
    | None -> raise Not_found
    | Some r -> (x, r)
  ) env 
  with Not_found -> []

  (* TODO: remove, and add [last_entry env] *)
  let rev_entries env = try List.map (fun (x, r) ->
    match !r with
    | None -> raise Not_found
    | Some r -> (x, r)
  ) env 
  with Not_found -> []

  let empty () = []

  let bind x v env = 
    match x with 
    | Unnamed -> env
    | Named x -> (x, ref (Some v)) :: env

  let declare x env = 
    match x with
    | Unnamed -> env
    | Named x -> (x, ref None) :: env

  (* Precondition: the identifier cannot be anonymous. *)
  let lookup_ref x env = 
    List.assoc x env

  exception DefiningUndeclaredVariable of value_identifier
  exception UndefinedVariable of value_identifier
  exception UndeclaredVariable of value_identifier
  exception CannotDefineAnonymous
  exception CannotLookupAnonymous

  let define x v env = 
    match x with
    | Unnamed -> raise CannotDefineAnonymous
    | Named x -> 
      try 
	(lookup_ref x env) := Some v
      with Not_found -> 
	raise (DefiningUndeclaredVariable x)

  let lookup x (env : 'a t) =
    match x with
    | Unnamed -> raise CannotLookupAnonymous
    | Named x ->
      try 
	match !(lookup_ref x env) with
	| Some x -> x
	| None -> raise (UndefinedVariable x)
      with Not_found -> 
	raise (UndeclaredVariable x)

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
let vunit = VStruct [ CIdentifier "U", None ]

open Pprint

let ( ++ ) d1 d2 = d1 ^^ space ^^ d2

let bracket e = group (text "[" ^^ e ^^ text "]")

let brace e = group (text "{" ^^ e ^^ text "}")

let print v = 
  let rec print = function
    | VInt x -> text (string_of_int x)
    | VChar c -> text "'" ^^ text (String.escaped (String.make 1 c)) ^^ text "'" 
    | VString s -> text "\"" ^^ text (String.escaped s) ^^ text "\""
    | VStruct [ (k, None) ] -> constructor_identifier k
    | VStruct [ (k, Some v) ] -> group (constructor_identifier k ++ bracket (print v))
    | VStruct kvs -> group (text "{" ^^ components kvs ++ text "}")
    | VClosure _ -> text "<code>"
    | VPrimitive _ -> text "<primitive>"

  and components cps = 
    sepmap (text "," ^^ break1) component cps

  and component (cid, e) =
    match e with 
    | None -> 
      constructor_identifier cid
    | Some v -> 
      group (constructor_identifier cid ++ text "<-" ++ nest 2 (group (print v)))

  and constructor_identifier (CIdentifier s) = text s
  in
  print v

let identifier (Identifier x) = text x

let vcat ds = sepmap hardline (fun x -> x) ds ^^ hardline

let print_env_identifier (i, v) =
    text ":-" ++ identifier i ++ text "=" ++ print v

let print_environment env =
  vcat (List.map (fun (i, v) -> 
    print_env_identifier(i, v)
  ) (Env.entries env))

let canonicalize s = 
  List.sort (fun (k, _) (k', _) -> compare k k') s

