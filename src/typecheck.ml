open AST
open Envtyp

let flag = ref false

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
          | DType(ty_id,ty_ids,ty)-> check_dtype ty_id ty_ids ty e
          | DVal(v) -> check_vdef v e 
        end in check defs e'

  and check_dtype ty_id ids ty e =
    failwith "DType Not implemented" 

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
    
     | ECase(ty,brs)	->
        begin match ty with
        | None   -> check_branchs brs e
        | Some p ->
            let t_ty = check_typ p e in
            let t_brs = check_branchs brs e in
            unification t_ty t_brs
        end

     | ESum(constr,ty_op,ex_op)	-> TSum([check_ESum constr ty_op ex_op e])
    
     | EProd(ty_op,l_pr)	-> TProd(check_EProd l_pr ty_op e)

  and check_mutually_recursive l e =
    let e' = 
        (* 1- declare empty functions *)
        List.fold_left (fun e' -> function
        | (Binding(i, _), _) -> bind i (typage "_alpha_" (compt ())) e')
        e l
    in
    (* 2- bind their bodies *)
    List.iter (function
      | (Binding(Named(i),_), body) -> 
            let t_body = check_expr body e' None  in
            define_ref i t_body e'
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
       let t_f = check_expr f e None in
        match t_f with
        | TArrow(ta_1,ta_2 )  -> 
            let tA = check_TArrow ta_1 ta_2 e1 e in
            begin match pr_ex with
            | None -> tA
            | Some p -> unification tA p
            end
        | TVar(TIdentifier("_alpha_",nb),_) when nb >= 0 -> check_TVar_alpha f t_f e1 e pr_ex 
        | TVar(TIdentifier(str,nb),_) ->  raise (EAppErrorTVar (str, nb))
        | TSum(_) -> failwith "EApp -> TSum Not implemented"
        | TProd(_) -> failwith "EApp -> TProd Not implemented"
        | TRec(_,_) -> failwith "EApp -> TRec Not implemented"

  and check_TArrow ta_1 ta_2 e1 e =
        let t_ex = check_expr e1 e (Some ta_1) in
        match (ta_1,t_ex) with
        | (t1,t2) when t1 = t2 -> ta_2
        | (TVar(TIdentifier("_alpha_",nb),_),t1) when nb >= 0 -> ta_2
        | (t1,TVar(TIdentifier("_alpha_",nb),_)) when nb >= 0 -> ta_2
        | (_, _) -> raise EAppErrorTyping 

  and check_TVar_alpha f t_f e1 e pr_ex =
          let ty_f = match pr_ex with
          | Some p -> p
          | None -> typage "_alpha_" (compt ())
          in
          match f with 
          |EVar(v) -> define_ref v (TArrow(t_f, ty_f)) e ;check_TArrow t_f ty_f e1 e            
          | _ -> failwith "EApp -> TVar function Error "

  and check_EFun i ty exp e pr_ex =
    match ty with
    | None    ->
        let ty'' = typage "_alpha_"  (compt ()) in
        let e' = bind i ty'' e in
        let t_ex = check_expr exp e' pr_ex in
        begin match i with
        | Unnamed -> raise EFunErrorTyping
        | Named v -> 
            begin match lookup_ref v e' with
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
                 define_ref v t_f e ; t_f
            | (Some t_f , _ ) ->
                 define_ref v t_f e ; t_f
            | (None,Some t_f) -> 
                 define_ref v t_f e ; t_f
            | (None,_) -> raise (UndeclaredVariable v)
            end

  and check_ESum constr ty_op ex_op e =
     match (ty_op,ex_op) with
        | (Some ty, Some ex) ->
            let t_ty = check_typ ty e in
            let t_ex = check_expr ex e None in
            TConstructor(constr , Some (unification t_ty t_ex) )
        | (None ,Some ex) ->
            let t_ex = check_expr ex e None in
            TConstructor(constr , Some t_ex )
        | (None ,None ) -> TConstructor(constr,None)
        | (_,_) -> raise ESumErrorTyping 

  and check_EProd l_pr ty_op e =
    match l_pr with
    | [] -> []
    | (constr,ex_op)::l_pr' -> 
        (check_ESum constr ty_op ex_op e)::(check_EProd l_pr' ty_op e)

  and check_branchs brs e =
    match brs with
    | []    -> raise BranchsErrorVide
    | Branch(p,ex)::[] -> check_branch p ex e
    | Branch(p,ex)::brs' ->
        let t_brs' = check_branchs brs' e in
        match check_branch p ex e with
        | t_brs when t_brs = t_brs' -> t_brs'
        | _ -> raise BranchErrorUnion

  and check_branch p ex e = check_pattern p ex e
    
  and check_pattern patt ex e =
    match patt with
    | PSum(_,_,_) -> failwith "Pattern Not Implemented"
    | PProd(_,_) -> failwith "Pattern Not Implemented"
    | PAnd(_,_) -> failwith "Pattern Not Implemented"
    | POr(_,_) -> failwith "Pattern Not Implemented"
    | PNot(_) -> failwith "Pattern Not Implemented"
    | PVar(v) -> 
        let e' = bind (Named v) (typage "_alpha_"  (compt ())) e in
        check_expr ex e' None
    | PZero -> typage "_alpha_"  (compt ())
    | POne -> typage "_alpha_"  (compt ())


  and check_typs tys e =
    match tys with
    | [] -> []
    | ty::tys' -> (check_typ ty e)::(check_typs tys' e) 

  and check_typ_constr ty_constr e =
    match ty_constr with
    | [] -> []
    | (TConstructor(c, Some ty))::tyc' -> 
        (TConstructor(c, Some (check_typ ty e)))::(check_typ_constr tyc' e) 
    | (TConstructor(_,_) as constr)::tyc' -> 
        (constr)::(check_typ_constr tyc' e) 

  and check_typ ty e =
    match ty with 
    | TVar(t_i,tys) -> 
        if Hashtbl.mem sign t_i 
        then TVar(t_i, check_typs tys e)
        else raise TVarErrorTyping
    | TArrow(t1,t2)     -> TArrow(check_typ t1 e, check_typ t2 e)
    | TSum(l_constr)    -> TSum(check_typ_constr l_constr e)
    | TProd(l_constr)   -> TProd(check_typ_constr l_constr e)
    | TRec(t_i,tr_ty)   -> 
        if Hashtbl.mem sign t_i 
        then TRec(t_i, (check_typ tr_ty e))
        else raise TVarErrorTyping

  in 
  if !flag then 
     (add_op_defauld () ; check p (empty ()) )
