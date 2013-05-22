open AST
open Operator

let flag = ref false

type value_type = TUnit | TInt | TChar | TString | TBool 
    | TCustom  of value_type
    | TFun of value_type * value_type

type env = (value_identifier * value_type option ref)list

let program p = 
  if !flag then 
    failwith "Students, this is your job."
