open AST

(* Type-checking exceptions *)
exception UndeclaredVariable of value_identifier
exception EFunTypingError
exception EAppTypingErrorTArrow
exception EAppTypingErrorOther
exception EAppTypingErrorTSum
exception EAppErrorTVar of string * int
exception PSumTypingError
exception TVarTypingError 
exception UnificationError
exception TSumTProdUnificationError
exception EmptyBranchError
exception UnionBranchError

(* Environment of typing *)
type env = (value_identifier * AST.typ option ref) list

(* Types signatures *)
let sign = Hashtbl.create 64

let add_op_default () =
  Hashtbl.add sign (TIdentifier("int",    -1 )) ();
  Hashtbl.add sign (TIdentifier("char",   -1 )) ();
  Hashtbl.add sign (TIdentifier("string", -1 )) ();
  Hashtbl.add sign (TIdentifier("bool",   -1 )) ();
  Hashtbl.add sign (TIdentifier("U",      -1 )) ()

(* Abstract types counter *)
let compteur = ref (-1)

(* Increment the abstract types counter and return the new value. *)
let compt () = incr compteur ; !compteur

let empty () = []

let bind x v env = 
  match x with 
    | Unnamed -> env
    | Named x -> (x, ref (Some v)) :: env

let declare x env = 
  match x with
  | Unnamed -> env
  | Named x -> (x, ref None) :: env

let identifier p = 
  let p = AST.EVar p in
  Operator.is_binop p || Operator.is_unop p

(* helpers *)
let ( --> ) x y = 
  match x with 
  | AST.EVar x -> (x, y)
  | _ -> assert false

let typage x y = TVar(TIdentifier(x,y), [])
let (--->) (x,z) (y,w) = TArrow( typage x z ,typage y w )
let (&-->) (x,z) y = TArrow(typage x z ,y )

let lookup x = List.assoc x [
  Operator.minus        --> (("int", -1)  &--> (("int", -1) ---> ("int", -1)));
  Operator.plus         --> (("int", -1)  &--> (("int", -1) ---> ("int", -1)));
  Operator.star         --> (("int", -1)  &--> (("int", -1) ---> ("int", -1)));
  Operator.slash        --> (("int", -1)  &--> (("int", -1) ---> ("int", -1)));
  Operator.percent      --> (("int", -1)  &--> (("int", -1) ---> ("int", -1)));
  Operator.eq           -->
    (("_alpha_", compt ()) &--> (("_alpha_", compt ()) ---> ("bool", -1)));
  Operator.bangeq       -->
    (("_alpha_", compt ()) &--> (("_alpha_", compt ()) ---> ("bool", -1)));
  Operator.andand       --> (("bool", -1) &--> (("bool", -1) --->("bool", -1)));
  Operator.pipepipe     --> (("bool", -1) &--> (("bool", -1) --->("bool", -1)));
  Operator.le           --> (("int", -1)  &--> (("int", -1) ---> ("bool",  -1)));
  Operator.ge           --> (("int", -1)  &--> (("int", -1) ---> ("bool",  -1)));
  Operator.lt           --> (("int", -1)  &--> (("int", -1) ---> ("bool",  -1)));
  Operator.gt           --> (("int", -1)  &--> (("int", -1) ---> ("bool",  -1)));
  Operator.boolean_not  --> (("bool", -1) ---> ("bool", -1));
  Operator.negate       --> (("int" , -1) ---> ("int",  -1));
]

let rec lookup_ref x env = 
  match env with
  | [] -> None
  | (x',t)::env' -> 
      if x = x' 
      then !t 
      else lookup_ref x env'

let rec define_ref x t env =
  match env with
    | [] -> ()
    | (x', t')::env' ->
        if x' = x
        then t' := (Some t )
        else define_ref x t env'

let tInt    = TVar(TIdentifier("int",    -1), [])
let tChar   = TVar(TIdentifier("char",   -1), [])
let tString = TVar(TIdentifier("string", -1), [])
let tBool   = TVar(TIdentifier("bool",   -1), [])
let tUnit   = TVar(TIdentifier("U",      -1), [])

(* Compares two TSum/TProd lists, and return their type *)
let list_compare l1 l2 ty = 
  if l1 = l2 then ty 
  else raise TSumTProdUnificationError

(* Unifies two types *)
let rec unification t1 t2 =
  match t1, t2 with
  | t1 , t2 when t1 = t2 -> t1
  | TVar(tI, _), TVar(tI_ex, _) when tI = tI_ex           -> t1
  | TVar(_, _), TVar(TIdentifier(s,nb), _) when (nb >= 0) -> t1
  | TVar(TIdentifier(s, nb), _), TVar(_, _) when (nb >= 0) -> t2
  | TArrow(a1, b1), TArrow(a2, b2) ->
      TArrow(unification a1 a2, unification b1 b2)
  | TSum(l1), TSum(l2) ->
      list_compare (List.sort compare l1)  (List.sort compare l2) t1
  | TProd(l1), TProd(l2) ->
      list_compare (List.sort compare l1)  (List.sort compare l2) t1
  | TRec(_, _), TRec(_, _) ->
      failwith "Unif TRec Not Implemented"
  | (_, _) ->
      raise UnificationError

