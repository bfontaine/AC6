open Ast
open Runtime

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
    let rec check_type p e = 
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
            end in check_type defs e'

    and check_vdef x env =
        match x match 
        | Simple(_, exp )       -> failwith "Simple Not implemented" 
        | MutuallyRecursive(_)  -> failwith "MutuallyRecursive Not implemented"

    and check_exp exp env = 
        match exp with
        | EChar(_)      -> failwith "EChar ot implemented" 
        | EInt(_)       -> failwith "EInt ot implemented" 
        | EString(_)    -> failwith "EString ot implemented" 
        | EAnnot(_,_)   -> failwith "EAnnot ot implemented" 
        | EApp(_,_)     -> failwith "EApp ot implemented" 
        | ESum(_,_,_)   -> failwith "ESum ot implemented" 
        | EProd(_,_)    -> failwith "EProd ot implemented" 
        | ECase(_,_)    -> failwith "ECase ot implemented" 
        | EFun(_,_)     -> failwith "EFun ot implemented" 
        | EDef(_,_)     -> failwith "EDef ot implemented" 
        | ESeq(_)       -> failwith "ESeq ot implemented" 
    in
      check_type p (e) 
