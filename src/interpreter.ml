open AST 
open Runtime

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv


let rec program p =

  (* evaluate a program with an environment *)
  let rec eval p e = match p with
    (* No definitions *)
    | [] -> e
    | d::defs ->
        (* evaluate the definition, and iter on the
         * rest of the program *)
        let e' = begin match d with
          | DType(_) -> e (* there's no type here *)
          | DVal(v) -> eval_vdef v e
        end in eval defs e'

  (* evaluate a vdefinition within an environment *)
  and eval_vdef v e = match v with
    | Simple(Binding(i, _), ex) ->
        Env.bind i (eval_expr ex e) e
    | MutuallyRecursive(_) -> failwith "Not implemented"

  (* evaluate an expression within an environment *)
  and eval_expr exp e = match exp with
    
    | EDef(v, exp2) ->
        eval_expr exp2 (eval_vdef v e)

    | EInt(i)    -> VInt(i)
    | EChar(c)   -> VChar(c)
    | EString(s) -> VString(s)

    | ESeq(es) -> 
        (* evaluate only the last expression *)
        eval_expr (List.nth es ((List.length es) - 1)) e

    | _ ->
        failwith "Not implemented"

  in
    eval p (Env.empty ())

