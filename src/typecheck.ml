open AST

let flag = ref false

(** Exception of check typing **)
exception UndeclaredVariable of value_identifier
exception SimpleErrorTyping
exception EAnnotErrorTypping
exception EAppErrorTyping
exception EVarErrorTyping



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


(** Abstract ariable counter*)
let compteur = ref 0

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
 Operator.eq          --> (("_alpha_",0)  &--> (("_alpha_",0) ---> ("bool",-1)));
 Operator.bangeq      --> (("_alpha_",0)  &--> (("_alpha_",0) ---> ("bool",-1)));
 Operator.andand      --> (("bool",-1)  &--> (("bool",-1) --->("bool",-1)));
 Operator.pipepipe    --> (("bool",-1)  &--> (("bool",-1) --->("bool",-1)));
 Operator.le          --> (("_alpha_",0)  &--> (("_alpha_",0) ---> ("bool",-1)));
 Operator.ge          --> (("_alpha_",0)  &--> (("_alpha_",0) ---> ("bool",-1)));
 Operator.lt          --> (("_alpha_",0)  &--> (("_alpha_",0) ---> ("bool",-1)));
 Operator.gt          --> (("_alpha_",0)  &--> (("_alpha_",0) ---> ("bool",-1)));
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

let iNT = TVar(TIdentifier("int",-1),[])
let cHAR = TVar(TIdentifier("char",-1),[])
let sTR = TVar(TIdentifier("string",-1),[])
let bOOL = TVar(TIdentifier("bool",-1),[])
let tUnit = TVar(TIdentifier("U",-1),[])

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
    | MutuallyRecursive(l) -> failwith "MutuallyRecursive Not implemented"

  and check_expr exp e pr_ex =
	 match exp with
     | EInt(_)	    -> iNT 
     | EChar(_)     -> cHAR
     | EString(_)   -> sTR
     | EVar(v)	    -> 
        if identifier v 
        then lookup v 
        else 
            begin match ((lookup_ref v e),pr_ex) with 
            | (Some t,Some pr_t) -> 
                begin match (t,pr_t) with
                | (TVar(TIdentifier("_alpha_",nb),_) ,pr_ex) -> pr_ex
                | ( t ,TVar(TIdentifier("_alpha_",nb),_))   -> t
                | (t1,t2) -> 
                    if t1 = t2 then t1
                    else raise EVarErrorTyping
                end
            | (Some t , _ ) -> t
            | (None,_) -> raise (UndeclaredVariable v)
            end
    
     | EAnnot(ex,ty)	->
        let t_ty = check_typ ty e in
        let t_ex = check_expr ex e (Some t_ty) in
        if t_ty = t_ex then t_ty
        else raise EAnnotErrorTypping
            
     | ESeq(es)     -> check_ESeq es e pr_ex 
     
     | EDef(v,exp2)    ->
        check_expr exp2 (check_vdef v e) pr_ex
     
     | EFun(Binding(i,ty),exp)    -> check_EFun i ty exp e pr_ex

     | EApp(f,e1)   -> check_EApp f e1 e pr_ex
    
     | ECase(_,_)	-> failwith "ECase Not implemented"

     | ESum(_,_,_)	-> failwith "ESum Not implemented"
     
     | EProd(_,_)	-> failwith "EProd Not implemented"
 
  and check_simple i ty ex e =
    let ty'= check_expr ex e None in
    begin match ty with 
     | None   -> bind i ty' e
     | Some p -> let ty'' = check_typ p e in
        if ty'' = ty' then bind i ty' e
        else raise SimpleErrorTyping
    end
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
        begin match t_f with
        | TArrow(t_1,t )  -> 
            let t_ex = check_expr e1 e (Some t_1) in
            begin match (t_1,t_ex) with
            | (TVar(TIdentifier("_alpha_",nb),_),t1) -> 
                if nb >= 0 then t 
                else raise EAppErrorTyping 
            | (t1,TVar(TIdentifier("_alpha_",nb),_)) -> 
                if nb >= 0 then t
                else raise EAppErrorTyping 
            | (t1,t2) ->
                if t1 = t2 then t
                else raise EAppErrorTyping 
            end
        | _     -> failwith "EApp Not implemented"
        end

  and check_EFun i ty exp e pr_ex =
    match ty with
    | None    ->
        let ty'' = typage "_alpha_"  (compt ()) in
        let e' = bind i ty'' e in
        TArrow( ty'',check_expr exp e' pr_ex)
    | Some t  ->
        let ty'' = check_typ t e in
        let e' = bind i ty'' e in
        TArrow(ty'', check_expr exp e' pr_ex )

  and check_typ ty e =
    match ty with 
    | TVar(_,_)     -> failwith "TVar Not implemented" 
    | TArrow(_,_)   -> failwith "TArrow Not implemented" 
    | TSum(_)       -> failwith "TSum Not implemented" 
    | TProd(_)      -> failwith "TProd Not implemented" 
    | TRec(_)       -> failwith "TRec Not implemented" 
  in 
  if !flag then 
     (add_op_defauld () ; check p (empty ()) )
