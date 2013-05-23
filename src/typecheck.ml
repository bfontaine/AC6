open AST

let flag = ref false

(** Exception of check typing **)
exception UndeclaredVariable of value_identifier
exception SimpleErrorTyping
exception EAnnotErrorTypping
exception EAppErrorTyping


(** Environment of typing  **)
type env = (value_identifier * AST.typ option ref)list

(** Signature of typping**)
let sign = Hashtbl.create 42

let add_op_defauld () =
    Hashtbl.add sign (TIdentifier("int"))  ();
    Hashtbl.add sign (TIdentifier("char"))  ();
    Hashtbl.add sign (TIdentifier("string"))  ();
    Hashtbl.add sign (TIdentifier("bool"))  ();
    Hashtbl.add sign (TIdentifier("U"))  ()


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

let (--->) x y = TArrow(TVar(TIdentifier(x),[]),TVar(TIdentifier(y),[]))
let (&-->) x y = TArrow(TVar(TIdentifier(x),[]),y)
let typage x = TVar(TIdentifier(x),[])
 
let lookup x = List.assoc x [
 Operator.minus       --> ("int"&-->("int" --->"int"));
 Operator.plus        --> ("int"&-->("int" --->"int"));
 Operator.star        --> ("int"&-->("int" --->"int"));
 Operator.slash       --> ("int"&-->("int" --->"int"));
 Operator.percent     --> ("int"&-->("int" --->"int"));
 Operator.eq          --> ("alpha0"&-->("alpha0" --->"bool"));
 Operator.bangeq      --> ("alpha0"&-->("alpha0" --->"bool"));
 Operator.andand      --> ("bool"&-->("bool" --->"bool"));
 Operator.pipepipe    --> ("bool"&-->("bool" --->"bool"));
 Operator.le          --> ("alpha0"&-->("alpha0" --->"bool"));
 Operator.ge          --> ("alpha0"&-->("alpha0" --->"bool"));
 Operator.lt          --> ("alpha0"&-->("alpha0" --->"bool"));
 Operator.gt          --> ("alpha0"&-->("alpha0" --->"bool"));
 Operator.negate      --> ("bool" --->"bool");
 Operator.boolean_not --> ("int" --->"int");
]

let rec lookup_ref x env = 
    match env with
    | [] -> None
    | (x',t)::env' -> 
        if x = x' 
            then !t 
        else lookup_ref x env'

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

  and check_expr exp e =
	 match exp with
     | EInt(_)	    -> typage "int" 
     | EChar(_)     -> typage "char"
     | EString(_)   -> typage "string"
     | EVar(v)	    -> 
        if identifier v 
        then lookup v 
        else 
            begin match (lookup_ref v e) with 
            | Some t -> t 
            | None -> raise (UndeclaredVariable v)
            end
    
     | EAnnot(ex,ty)	->
        let t_ty = check_typ ty e in
        let t_ex = check_expr ex e in
        if t_ty = t_ex then t_ty
        else raise EAnnotErrorTypping
            
     | ESeq(es)     -> check_eseq es e
     
     | EDef(v,exp2)    ->
        check_expr exp2 (check_vdef v e)
     
     | EFun(Binding(i,ty),exp)    -> 
        begin match ty with
        | None    ->
            let e' = declare i e in
            TArrow(typage ("alpha"^(string_of_int( compt ()))),check_expr exp e')
        | Some t  ->
            let ty'' = check_typ t e in
            let e' = bind i ty'' e in
            TArrow(ty'', check_expr exp e')
        end
     | EApp(f,e1)    -> 
        let t_f = check_expr f e in
        begin match t_f with
        | TArrow(t_1,t )  -> 
            if t_1 = (check_expr e1 e) then t
            else raise EAppErrorTyping 
        | _     -> failwith "EApp Not implemented"
        end
     
     | ECase(_,_)	-> failwith "ECase Not implemented"

     | ESum(_,_,_)	-> failwith "ESum Not implemented"
     
     | EProd(_,_)	-> failwith "EProd Not implemented"
 
  and check_simple i ty ex e =
    let ty'= check_expr ex e in
    begin match ty with 
     | None   -> bind i ty' e
     | Some p -> let ty'' = check_typ p e in
        if ty'' = ty' then bind i ty' e
        else raise SimpleErrorTyping
    end
  and check_eseq es e =
      match es with
        | [] -> TVar(TIdentifier"U",[])
        | [ex] ->
          check_expr ex e
        | ex::es' ->
          let _ = check_expr ex e in
             check_eseq es' e

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
