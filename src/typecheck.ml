open AST
open Operator

let flag = ref false

type value_type = TUnit | TInt | TChar | TString | TBool 
    | TCustom  of value_type
    | TFun of value_type * value_type

type env = (value_identifier * value_type option ref)list

let empty () = []

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
      | d::vdef -> failwith "Defs Not implemented" 

  in 
  if !flag then 
     check p (empty ())
