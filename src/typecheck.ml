open AST
open Operator

let flag = ref false

let env = (value_identifier * value_type option ref)list

let value_type = 
    | TInt 
    | TChar
    | TString
    | TBool
    | TCustom value_type
    | TFun value_type * value_type


let program p = 
  if !flag then 
    failwith "Students, this is your job."
