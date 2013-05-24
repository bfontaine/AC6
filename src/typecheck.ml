open AST

let flag = ref false

(** Exception of check typing **)
exception UndeclaredVariable of value_identifier
exception SimpleErrorTyping
exception EFunErrorTyping
exception EAnnotErrorTyping
exception EAppErrorTyping
exception EAppErrorTVar of string * int
exception EVarErrorTyping
exception TVarErrorTyping 
exception UnificationError

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

(*** Unification of typing ***)
module MapUnif = Map.Make(struct
    type t = int  
    let compare = compare
end)

let rec unification ty ty_exp =
    match ty, ty_exp with
    | TVar(tI,_), TVar(tI_ex,_) when tI = tI_ex-> ty 
    | TVar(_,_), TVar(TIdentifier(s,nb),_) when (nb >= 0) -> ty 
    | TVar(TIdentifier(s,nb),_), TVar(_,_) when (nb >= 0) -> ty_exp 
    | TArrow(a1,b1), TArrow(a2,b2)  -> TArrow(unification a1 a2, unification b1 b2)
    | TSum(_), TSum(_)          -> failwith "Unif TSum Not Implemented"
    | TProd(_), TProd(_)        -> failwith "Unif TProd Not Implemented"
    | TRec(_,_), TRec(_,_)      -> failwith "Unif TRec Not Implemented"
    |(_,_)                      -> raise UnificationError
(**
 * Check a program.
 *
 * @param p the program (list of definitions)
 * @return an unit
 **)
let program p = 
  
 (**
  * Check a program with an environment of typing.
  *
  * @param p the program (list of definitions)
  * @param e the environment
  * @return the new environment
  **)
 let rec check p e =
      match p with
      (* No definitions *)
      | [] -> ()
      (* One or more definitions *)
      | d::defs -> 
        (* Check the definition, and iter on the
         * rest of the program *)
        let e' = begin match d with
          | DType(ty_id,ty_ids,ty)-> failwith "DType Not implemented"
          | DVal(v) -> check_vdef v e 
        end in check defs e'

  and check_vdef v e =
    match v with
    | Simple(Binding(i, ty), ex) -> check_simple i ty ex e 
    | MutuallyRecursive(l) -> check_mutually_recursive l e

  and check_expr exp e pr_ex =
	 match exp with
     | EInt(_)	    -> tInt 
     | EChar(_)     -> tChar
     | EString(_)   -> tString
     | EVar(v)	    -> check_EVar v e pr_ex
     | EAnnot(ex,ty)	->
        let t_ty = check_typ ty e in
        let t_ex = check_expr ex e (Some t_ty) in
        unification t_ty t_ex

     | ESeq(es)     -> check_ESeq es e pr_ex 
     
     | EDef(v,exp2)    ->
        check_expr exp2 (check_vdef v e) pr_ex
     
     | EFun(Binding(i,ty),exp)    -> check_EFun i ty exp e pr_ex

     | EApp(f,e1)   -> check_EApp f e1 e pr_ex
    
     | ECase(_,_)	-> failwith "ECase Not implemented"

     | ESum(_,_,_)	-> failwith "ESum Not implemented"
     
     | EProd(_,_)	-> failwith "EProd Not implemented"


  and check_mutually_recursive l e =
    let e' = 
        (* 1- declare empty functions *)
        List.fold_left (fun e' -> function
        | (Binding(i, _), _) -> declare i e')
        e l
    in
    (* 2- bind their bodies *)
    List.iter (function
      | (Binding(Named(i), ty), body) -> 
            let t_body = check_expr body e' None  in
            let t_ty = match ty with 
                | None -> t_body  
                | Some p -> check_typ p e' 
            in
            let t_body_fn = unification t_ty t_body in
            define_ref i t_body_fn e'
      | _ -> ()
    ) l; e'


  and check_simple i ty ex e =
    let ty_ex = check_expr ex e None in
    match ty with 
     | None   -> bind i ty_ex e
     | Some p -> let ty' = check_typ p e in
        let ty_fi = unification ty' ty_ex in 
            bind i ty_fi e

  and check_ESeq es e  pr_ex =
      match es with
        | [] -> tUnit
        | [ex] ->
          check_expr ex e pr_ex
        | ex::es' ->
          let _ = check_expr ex e pr_ex in
             check_ESeq es' e pr_ex

  and check_EApp f e1 e pr_ex =
       let t_f = check_expr f e pr_ex in
        match t_f with
        | TArrow(ta_1,ta_2 )  -> check_TArrow ta_1 ta_2 e1 e 
        | TVar(TIdentifier(str,nb),_) ->  raise (EAppErrorTVar (str, nb))
        | TSum(_) -> failwith "EApp -> TSum Not implemented"
        | TProd(_) -> failwith "EApp -> TProd Not implemented"
        | TRec(_,_) -> failwith "EApp -> TRec Not implemented"

  and check_TArrow ta_1 ta_2 e1 e =
        let t_ex = check_expr e1 e (Some ta_1) in
        match (ta_1,t_ex) with
        | (TVar(TIdentifier("_alpha_",nb),_),t1) when nb >= 0 -> ta_2
        | (t1,TVar(TIdentifier("_alpha_",nb),_)) when nb >= 0 -> ta_2
        | (t1,t2) when t1 = t2 -> ta_2
        | (_, _) -> raise EAppErrorTyping 

  and check_EFun i ty exp e pr_ex =
    match ty with
    | None    ->
        let ty'' = typage "_alpha_"  (compt ()) in
        let e' = bind i ty'' e in
        let t_ex = check_expr exp e' pr_ex in
        begin match i with
        | Unnamed -> TArrow(ty'', t_ex)
        | Named v -> begin match lookup_ref v e' with
            | Some t -> TArrow(t, t_ex)
            | None -> raise EFunErrorTyping
            end
        end
    | Some t  ->
        let ty'' = check_typ t e in
        let e' = bind i ty'' e in
        TArrow(ty'', check_expr exp e' pr_ex)

  and check_EVar v e pr_ex =
        if identifier v 
        then lookup v 
        else 
            begin match ((lookup_ref v e),pr_ex) with 
            | (Some t,Some pr_t) -> 
                 let t_f = unification t pr_t in
                 define_ref v t e ; t_f
            | (Some t_f , _ ) ->
                 define_ref v t_f e ; t_f
            | (None,_) -> raise (UndeclaredVariable v)
            end

  and check_typ ty e =
    match ty with 
    | TVar(t_i,_) -> 
        if Hashtbl.mem sign t_i then ty
        else raise TVarErrorTyping
    | TArrow(t1,t2)   -> TArrow(check_typ t1 e, check_typ t2 e) 
    | TSum(_)       -> failwith "TSum Not implemented" 
    | TProd(_)      -> failwith "TProd Not implemented" 
    | TRec(_)       -> failwith "TRec Not implemented" 
  in 
  if !flag then 
     (add_op_defauld () ; check p (empty ()) )
