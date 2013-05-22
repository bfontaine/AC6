open Ast
open Runtime


type env = (value_identifier * type_value option ref ) list

type type_value = 
| TInt
| TChar
| TString
| TBool
| TCustom of string 
| TFun of type_value * type_value

let empty () = []

let bind x t env = 
  match x with 
  | Unnamed -> env
  | Named x -> (x, ref (Some t)) :: env

let declare x env = 
  match x with
  | Unnamed -> env
  | Named x -> (x, ref None) :: env


let flag = ref false

(**
 * Check type a program.
 *
 * @param p the program (list of definitions)
 * @return unit
 * *)
let program p = 

  if !flag then 
   (**
    * Check type a program with an environment of type.
    * @param p the program (list of definitions)
    * @param e the environment
    * @return unit
    **)
    let rec check_defs p e = 
        match p with
        (* No definitions *)
        | [] -> e
        (* One or more definitions *)
        | d::defs ->
            (* evaluate the definition, and iter on the
             * rest of the program *)
            let e' = begin match d with
            | DType(_) -> failwith "DType Not implemented"   
            | DVal(v)  -> check_vdef v e
            end in (check_defs defs e')

    and check_vdef v env =
        match v match 
        | Simple(Binding(arg,ty), exp )    -> let ty' = 
            begin match ty with 
            | None -> eval_exp exp env 
            | Some x -> failwith "typ Not implemented" 
            end 
             in bind arg ty' env
        | MutuallyRecursive(_)  -> failwith "MutuallyRecursive Not implemented"


    and check_exp exp env = 
        match exp with
        | EChar(c)      -> TChar 
        | EInt(i)       -> TInt
        | EString(s)    -> TString
        | EAnnot(ex,ty) -> failwith "EAnnot ot implemented" 
        | EApp(_,_)     -> failwith "EApp ot implemented" 
        | ESum(_,_,_)   -> failwith "ESum ot implemented" 
        | EProd(_,_)    -> failwith "EProd ot implemented" 
        | ECase(_,_)    -> failwith "ECase ot implemented" 
        | EFun(_,_)     -> failwith "EFun ot implemented" 
        | EDef(_,_)     -> failwith "EDef ot implemented" 
        | ESeq(_)       -> failwith "ESeq ot implemented" 

   and check_typ ty env =
        match ty with 
        (** Type constructor application. *)
        | TVar(_,_) ->    
        (** Function types.               *) 
        | TArrow(_,_)     
        (** Sum types.                    *)
        | TSum(_)            
        (** Product types.                *)
        | TProd(_)           
        (** Recursive types.              *)
        | TRec(_,_)


    in
      check_program p (empty ()) 
