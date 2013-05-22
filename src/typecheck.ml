open AST
open Operator

let flag = ref false

type value_type = 
    | TUnit 
    | TInt 
    | TChar 
    | TString 
    | TBool 
    | TCustom  of value_type
    | TFun of value_type * value_type

type env = (value_identifier * value_type option ref)list

let empty () = []

let bind x v env = 
    match x with 
    | Unnamed -> env
    | Named x -> (x, ref (Some v)) :: env

let declare x env = 
  match x with
  | Unnamed -> env
  | Named x -> (x, ref None) :: env

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
     | EVar(_)	    -> failwith "EVar Not implemented"
     | ESum(_,_,_)	-> failwith "ESum Not implemented"
     | EProd(_,_)	-> failwith "EProd Not implemented"
     | EAnnot(_,_)	-> failwith "EAnnot Not implemented"
     | ESeq(_)	    -> failwith "ESeq Not implemented" 
     | EDef(_,_)    -> failwith "EDef Not implemented"
     | EApp(_,_)    -> failwith "EApp Not implemented"
     | ECase(_,_)	-> failwith "ECase Not implemented"
     | EFun(_,_)    -> failwith "EFun Not implemented" 

  in 
  if !flag then 
     check p (empty ())
