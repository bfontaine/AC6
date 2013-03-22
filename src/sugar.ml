open AST

let mk_ifthenelse e1 e2 e3 = 
  EApp (ECase (None, 
	       [
		 Branch (PSum (CIdentifier "True", None, None), e2);
		 Branch (PSum (CIdentifier "False", None, None), e3);
	       ]), 
	e1)

let unitid = 
  CIdentifier "U"
  
let unitty = 
  TSum [TConstructor (unitid, None)]

let eunit = 
  ESum (unitid, Some unitty, None)

let mk_ifthen e1 e2 = 
  mk_ifthenelse e1 e2 eunit

let mk_fun bs rty body = 
  let e = match rty with None -> body | Some ty -> EAnnot (body, ty) in
  List.fold_left (fun f b -> EFun (b, f)) e (List.rev bs)

let mk_postfix_application e1 e2 =
  EApp (e2, e1)

let mk_where e vdef = 
  EDef (vdef, e)

let mk_do e = 
  mk_fun [Binding (Unnamed, Some unitty)] None e

let mk_fundef bs ty e = 
  match bs with
  | [] -> mk_fun [ Binding (Unnamed, Some unitty) ] ty e
  | _ -> mk_fun bs ty e
