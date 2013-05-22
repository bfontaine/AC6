open AST

let flag = ref false

type value_type = 
    | TUnit 
    | TInt 
    | TChar 
    | TString 
    | TBool 
    | TCustom  of value_type
    | TFun of value_type * value_type
    | TAbstrait of int

type env = (value_identifier * value_type option ref)list

let empty () = []

let compteur = ref 0

let compt () = incr compteur ; !compteur

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
 
let lookup x = List.assoc x [
 Operator.minus       --> TFun(TInt,TFun(TInt,TInt));
 Operator.plus        --> TFun(TInt,TFun(TInt,TInt)); 
 Operator.star        --> TFun(TInt,TFun(TInt,TInt)); 
 Operator.slash       --> TFun(TInt,TFun(TInt,TInt));     
 Operator.percent     --> TFun(TInt,TFun(TInt,TInt)); 
 Operator.eq          --> TFun(TAbstrait(0),TFun(TAbstrait(0),TBool)); 
 Operator.bangeq      --> TFun(TAbstrait(0),TFun(TAbstrait(0),TBool)); 
 Operator.andand      --> TFun(TBool,TFun(TBool,TBool)); 
 Operator.pipepipe    --> TFun(TBool,TFun(TBool,TBool)); 
 Operator.le          --> TFun(TAbstrait(0),TFun(TAbstrait(0),TBool));
 Operator.ge          --> TFun(TAbstrait(0),TFun(TAbstrait(0),TBool));
 Operator.lt          --> TFun(TAbstrait(0),TFun(TAbstrait(0),TBool));
 Operator.gt          --> TFun(TAbstrait(0),TFun(TAbstrait(0),TBool));
 Operator.negate      --> TFun(TInt,TInt); 
 Operator.boolean_not --> TFun(TBool,TBool);   
]

exception UndeclaredVariable of value_identifier

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
    | Simple(Binding(i, ty), ex) -> bind i (check_expr ex e) e
    | MutuallyRecursive(l) -> failwith "MutuallyRecursive Not implemented"

  and check_expr exp e =
	 match exp with
     | EInt(_)	    -> TInt
     | EChar(_)     -> TChar
     | EString(_)   -> TString
     | EVar(v)	    -> 
        if identifier v 
        then lookup v 
        else 
            begin match (lookup_ref v e) with 
            | Some t -> t 
            | None -> raise (UndeclaredVariable v)
            end
     | ESum(_,_,_)	-> failwith "ESum Not implemented"
     | EProd(_,_)	-> failwith "EProd Not implemented"
     | EAnnot(_,_)	-> failwith "EAnnot Not implemented"
     | ESeq(es)     -> check_eseq es e
     | EDef(v,exp2)    ->
        check_expr exp2 (check_vdef v e)
     | EApp(_,_)    -> failwith "EApp Not implemented"
     | ECase(_,_)	-> failwith "ECase Not implemented"
     | EFun(_,_)    -> failwith "EFun Not implemented" 

  and check_eseq es e =
      match es with
        | [] -> TUnit
        | [ex] ->
          check_expr ex e
        | ex::es' ->
          let _ = check_expr ex e in
             check_eseq es' e



  in 
  if !flag then 
     check p (empty ())
