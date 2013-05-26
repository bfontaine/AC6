open AST


(** Exception of check typing **)
exception UndeclaredVariable of value_identifier
exception SimpleErrorTyping
exception EFunErrorTyping
exception EAnnotErrorTyping
exception EAppErrorTyping
exception EAppErrorTVar of string * int
exception EVarErrorTyping
exception PSumErrorTyping
exception DTypeErrorSig
exception TVarErrorTyping 
exception UnificationError
exception UnificationError_TS_TP
exception BranchsErrorVide
exception BranchErrorUnion

(** Environment of typing  **)
type env = (value_identifier * AST.typ option ref)list

(** Signature of typping**)
let sign = Hashtbl.create 42

let add_op_defauld () =
    Hashtbl.add sign (TIdentifier("int",-1))  ();
    Hashtbl.add sign (TIdentifier("char",-1))  ();
    Hashtbl.add sign (TIdentifier("string",-1))  ();
    Hashtbl.add sign (TIdentifier("bool",-1))  ();
    Hashtbl.add sign (TIdentifier("U",-1))  ()


(** Abstract variable counter*)
let compteur = ref (-1)

let compt () = incr compteur ; !compteur

(** Function on environment**)
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

let ( --> ) x y = 
  match x with 
  | AST.EVar x -> (x, y)
  | _ -> assert false

let typage x y = TVar(TIdentifier(x,y), [])
let (--->) (x,z) (y,w) = TArrow( typage x z ,typage y w )
let (&-->) (x,z) y = TArrow(typage x z ,y )
 
let lookup x = List.assoc x [
 Operator.minus       --> (("int",-1)   &--> (("int",-1) ---> ("int",-1)));
 Operator.plus        --> (("int",-1)   &--> (("int",-1) ---> ("int",-1)));
 Operator.star        --> (("int",-1)   &--> (("int",-1) ---> ("int",-1)));
 Operator.slash       --> (("int",-1)   &--> (("int",-1) ---> ("int",-1)));
 Operator.percent     --> (("int",-1)   &--> (("int",-1) ---> ("int",-1)));
 Operator.eq          --> (("_alpha_",compt ())  &--> (("_alpha_",compt ()) ---> ("bool",-1)));
 Operator.bangeq      --> (("_alpha_",compt ())  &--> (("_alpha_",compt ()) ---> ("bool",-1)));
 Operator.andand      --> (("bool",-1)  &--> (("bool",-1) --->("bool",-1)));
 Operator.pipepipe    --> (("bool",-1)  &--> (("bool",-1) --->("bool",-1)));
 Operator.le          --> (("int",-1)  &--> (("int",-1) ---> ("bool",-1)));
 Operator.ge          --> (("int",-1)  &--> (("int",-1) ---> ("bool",-1)));
 Operator.lt          --> (("int",-1)  &--> (("int",-1) ---> ("bool",-1)));
 Operator.gt          --> (("int",-1)  &--> (("int",-1) ---> ("bool",-1)));
 Operator.boolean_not --> (("bool", -1) ---> ("bool",-1));
 Operator.negate      --> (("int" , -1) ---> ("int",-1));
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
        if x' = x then t' := (Some t )
        else define_ref x t env'

let tInt = TVar(TIdentifier("int",-1),[])
let tChar = TVar(TIdentifier("char",-1),[])
let tString = TVar(TIdentifier("string",-1),[])
let tBool = TVar(TIdentifier("bool",-1),[])
let tUnit = TVar(TIdentifier("U",-1),[])

let list_compare l_1 l_2 ty = 
    if l_1 = l_2 then ty 
    else raise UnificationError_TS_TP

let rec unification ty ty_exp =
    match ty, ty_exp with
    | ty , ty_exp when ty = ty_exp -> ty
    | TVar(tI,_), TVar(tI_ex,_) when tI = tI_ex-> ty 
    | TVar(_,_), TVar(TIdentifier(s,nb),_) when (nb >= 0) -> ty 
    | TVar(TIdentifier(s,nb),_), TVar(_,_) when (nb >= 0) -> ty_exp 
    | TRec(_,_), TRec(_,_)      -> failwith "Unif TRec Not Implemented"
    | TArrow(a1,b1), TArrow(a2,b2)  -> TArrow(unification a1 a2, unification b1 b2)
    | TSum(l_CI_1), TSum(l_CI_2) -> 
           list_compare (List.sort compare l_CI_1)  (List.sort compare l_CI_2) ty
    | TProd(l_CI_1), TProd(l_CI_2)  ->
           list_compare (List.sort compare l_CI_1)  (List.sort compare l_CI_2) ty
    |(_,_)                      -> raise UnificationError

