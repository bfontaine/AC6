open Ast
open Runtime

(* type env = ?  ?*)

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
            | DVal(_)  -> failwith "DVal  Not implemented"
            end in check_type defs e'
    in
      check_type p (e) 
