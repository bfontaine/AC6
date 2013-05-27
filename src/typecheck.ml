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
  let rec check prog envt =
    match prog with
      (* No definitions *)
      | [] -> ()
      (* One or more definitions *)
      | d::defs ->
          (* Check the definition, and iter on the
           * rest of the program *)
          let envt' = begin match d with
            | DType(type_id, type_ids, ty) ->
                check_dtype type_id type_ids ty envt
            | DVal(v) ->
                check_vdef v envt
          end in check defs envt'


  (* Evaluates and verifies a type declaration. *)
  and check_dtype type_id type_ids ty envt =
    failwith "DType Not implemented"


  (* Check the types of a VDefinition *)
  and check_vdef vdef envt =
    match vdef with
    | Simple(Binding(i, ty), ex) -> check_simple i ty ex envt
    | MutuallyRecursive(l)       -> check_mutually_recursive l envt

  (* Check the type of an expression given a predicted type, for example
     in ( E : int ), the type of E is "predicted" to be 'int'. *)
  and check_expr exp envt predicted_type =
    match exp with
     | EInt _         -> tInt
     | EChar _        -> tChar
     | EString _      -> tString
     | EVar(v)        -> check_EVar v envt predicted_type
     | EAnnot(ex, ty) ->
         let t_ty = check_typ ty envt in
           let t_ex = check_expr ex envt (Some t_ty) in
             unification t_ty t_ex

     | ESeq(es)       -> check_ESeq es envt predicted_type

     | EDef(vdef, exp2)  ->
         check_expr exp2 (check_vdef vdef envt) predicted_type

     | EFun(Binding(i, ty), exp) -> check_EFun i ty exp envt predicted_type

     | EApp(f, e)     -> check_EApp f e envt predicted_type

     | ECase(ty, branchs) ->
        begin match ty with
        | None   -> check_branchs branchs envt
        | Some p ->
            let t_ty = check_typ p envt in
              let t_branchs = check_branchs branchs envt in
                unification t_ty t_branchs
        end

      | ESum(constr, ty_op, ex_op) ->
          TSum([check_ESum constr ty_op ex_op envt])

      | EProd(ty_op, l_pr) -> TProd(check_EProd l_pr ty_op envt)


  and check_mutually_recursive l e =
    let e' =
      (* 1- declare empty functions *)
      List.fold_left (fun e' -> function
          (Binding(i, _), _) -> bind i (typage "_alpha_" (compt ())) e') e l
        in
        (* 2- bind their bodies' types *)
        List.iter (function
          (Binding(Named(i), _), body) ->
            let t_body = check_expr body e' None in
            define_ref i t_body e'
        | _ -> ()
        ) l; e'


  (* Check the type of a simple definition (val x = ...) *)
  and check_simple i ty ex e =
    let ty_ex = check_expr ex e None in
      match ty with
      | None   -> bind i ty_ex e
      | Some p -> let ty' = check_typ p e in
        let ty_fi = unification ty' ty_ex in
          bind i ty_fi e


  (* Check the type of a ESeq *)
  and check_ESeq es e pr_ex =
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
        | TArrow(ta_1, ta_2 )  ->
          let tA = check_TArrow ta_1 ta_2 e1 e in
            begin match pr_ex with
            | None -> tA
            | Some p -> unification tA p
            end
        | TVar(TIdentifier("_alpha_", nb), _) when nb >= 0 ->
            check_TVar_alpha f t_f e1 e pr_ex
        | TVar(TIdentifier(str, nb), _) ->  raise (EAppErrorTVar (str, nb))
        | _  -> failwith "EAppTypingErrorOther"

  and check_TArrow ta_1 ta_2 e1 e =
    let t_ex = check_expr e1 e (Some ta_1) in
      match (ta_1, t_ex) with
      | (t1, t2) when t1 = t2 -> ta_2
      | (TVar(TIdentifier("_alpha_", nb), _), t1) when nb >= 0 -> ta_2
      | (t1, TVar(TIdentifier("_alpha_", nb), _)) when nb >= 0 -> ta_2
      | (TSum(list_ts),TSum([typ_constr])) -> 
            if List.mem typ_constr list_ts then ta_2
            else raise EAppTypingErrorTSum
      | (_, _) -> raise EAppTypingErrorTArrow

  and check_TVar_alpha f t_f e1 e pr_ex =
    let ty_f = match pr_ex with
      | Some p -> p
      | None -> typage "_alpha_" (compt ())
    in
      match f with
      |EVar(v) ->
          define_ref v (TArrow(t_f, ty_f)) e ;
          check_TArrow t_f ty_f e1 e
      | _ -> failwith "EApp -> TVar function Error "

  and check_EFun i ty exp e pr_ex =
    match ty with
    | None ->
      let ty'' = typage "_alpha_"  (compt ()) in
        let e' = bind i ty'' e in
          let t_ex = check_expr exp e' pr_ex in
            begin match i with
            | Unnamed -> raise EFunTypingError
            | Named v ->
              begin match lookup_ref v e' with
              | Some t -> TArrow(t, t_ex)
              | None -> raise EFunTypingError
              end
            end
    | Some t ->
        let ty'' = check_typ t e in
          let e' = bind i ty'' e in
            TArrow(ty'', check_expr exp e' pr_ex)

  and check_EVar v e pr_ex =
    if identifier v
    then lookup v
    else begin match ((lookup_ref v e), pr_ex) with
        | (Some t, Some pr_t) ->
            let t_f = unification t pr_t in
            define_ref v t_f e ; t_f
        | (Some t_f , _ ) ->
            define_ref v t_f e ; t_f
        | (None, Some t_f) ->
            define_ref v t_f e ; t_f
        | (None, _) -> raise (UndeclaredVariable v)
    end

  and check_ESum constr ty_op ex_op e =
    match (ty_op, ex_op) with
    | (Some ty, Some ex) ->
        let t_ty = check_typ ty e in
        let t_ex = check_expr ex e None in
        TConstructor(constr , Some (unification t_ty t_ex) )
    | (None , Some ex) ->
        let t_ex = check_expr ex e None in
        TConstructor(constr , Some t_ex )
    | (Some ty , None ) -> TConstructor(constr,None )
    | (None , None ) -> TConstructor(constr, None)

  and check_EProd l_pr ty_op e =
    match l_pr with
    | [] -> []
    | (constr, ex_op)::l_pr' ->
        (check_ESum constr ty_op ex_op e)::(check_EProd l_pr' ty_op e)

  and check_branchs branchs envt =
    match branchs with
    | []    -> raise EmptyBranchError
    | Branch(pattern, expr)::[] -> check_branch pattern expr envt
    | Branch(pattern, expr)::branchs' ->
        let typ_branchs' = check_branchs branchs' envt in
          let typ_branch = check_branch pattern expr envt in
          union_branch typ_branch typ_branchs'

  and union_branch typ_branch typ_branchs =
    match  (typ_branch ,typ_branchs) with 
    | (TArrow(TSum(list_br),typ_exp) , TArrow(TSum(list_brs),typ_exp')) ->
        let typ_exp = unification typ_exp typ_exp' in
        let list_final = list_br@list_brs in
        TArrow(TSum(list_final),typ_exp)
    | _ -> raise UnionBranchError


  and check_branch pattern expr envt =
    let typ_patt = check_pattern pattern envt in 
    let typ_exp  = check_expr expr envt None in
    TArrow(typ_patt, typ_exp)

  and check_pattern pattern envt =
    match pattern with
    | PSum(constr, typ_op, pattern_op) ->
        TSum([check_PSum constr typ_op pattern_op envt])
    | PProd(typ_op, list_pr) ->
        TProd(check_PProd list_pr typ_op envt)
    | PAnd(p1, p2) ->
        unification (check_pattern p1 envt) (check_pattern p1 envt)
    | POr(p1, p2) ->
        unification (check_pattern p1 envt) (check_pattern p1 envt)
    | PNot(pattern_not) -> 
        check_pattern pattern_not envt
    | PVar(value_id) ->
        begin match (lookup_ref value_id envt) with
        | Some typ_value -> typ_value
        | None ->
            typage "_alpha_" (compt ())
        end
    | PZero ->
        typage "_alpha_"  (compt ())
    | POne ->
        typage "_alpha_"  (compt ())

  and check_PSum constr typ_op pattern_op envt =
    match (typ_op, pattern_op) with
    | (Some ty, Some pattern) ->
        let typ_ty = check_typ ty envt in
          let typ_patt = check_pattern pattern envt in
            TConstructor(constr, Some (unification typ_ty typ_patt))
    | (None , Some patt) ->
        let t_patt = check_pattern patt envt in
          TConstructor(constr, Some t_patt )
    | (Some ty, None) -> 
        TConstructor(constr, Some( check_typ ty envt))
    | (None , None ) -> TConstructor(constr, None)

  and check_PProd list_pr typ_op envt =
    match list_pr with
    | [] -> []
    | (constr, patt_op)::list_pr' ->
        let t_sum = check_PSum constr typ_op patt_op envt in
        (t_sum)::(check_PProd list_pr' typ_op envt)

  and check_typs tys e =
    match tys with
    | [] -> []
    | ty::tys' -> (check_typ ty e)::(check_typs tys' e)

  and check_typ_constr ty_constr e =
    match ty_constr with
    | [] -> []
    | (TConstructor(c, Some ty))::tyc' ->
        (TConstructor(c, Some (check_typ ty e)))::(check_typ_constr tyc' e)
    | (TConstructor(_, _) as constr)::tyc' ->
        (constr)::(check_typ_constr tyc' e)

  and check_typ ty e =
    match ty with
    | TVar(t_i, tys) ->
        if Hashtbl.mem sign t_i
        then TVar(t_i, check_typs tys e)
        else raise TVarTypingError
    | TArrow(t1, t2)   -> TArrow(check_typ t1 e, check_typ t2 e)
    | TSum(l_constr)   -> TSum(check_typ_constr l_constr e)
    | TProd(l_constr)  -> TProd(check_typ_constr l_constr e)
    | TRec(t_i, tr_ty) ->
        if Hashtbl.mem sign t_i
        then TRec(t_i, (check_typ tr_ty e))
        else raise TVarTypingError

  in
    if !flag then
      (add_op_default () ; check p (empty ()) )
