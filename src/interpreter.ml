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
    | MutuallyRecursive(_) ->
        failwith "MutuallyRecursive Not implemented"

  (* evaluate an expression within an environment *)
  (* FIXME This function returns an evaluated expression, but these
   *       expressions can modify the environment (e.g. 'x:=42'), so
   *       it should return an evironment with the evaluated expression.
   *)
  and eval_expr exp e = match exp with

    | EAnnot(exp2, _) ->
        eval_expr exp2 e

    | EApp(f, e1) ->
        let fn, e2 = (eval_expr f e), (eval_expr e1 e) in
          begin match fn with
          | VPrimitive(p) ->
              Primitive.apply p e2
          
          | VInt(_)
          | VChar(_)
          | VString(_)
          | VStruct(_) ->
              raise Primitive.InvalidPrimitiveCall

          | VClosure(_) ->
              failwith "VClosure Not Implemented"
          end

    (* case (and if/then/else) *)
    | ECase(_, branchs) ->
        failwith "ECase Not Implemented"

    (* chars *)
    | EChar(c)   -> VChar(c)

    | EDef(v, exp2) ->
        eval_expr exp2 (eval_vdef v e)

    (* function *)
    | EFun(arg, exp2) ->
        failwith "EFun not Implemented"

    (* ints *)
    | EInt(i)    -> VInt(i)

    (* product constructors *)
    | EProd(_, cl) ->
        failwith "EProd Not Implemented"

    (* strings *)
    | EString(s) -> VString(s)

    (* sum contructors *)
    | ESum(c, _, Some e1) ->
        let e1' = eval_expr e1 e
        in VStruct([(c, Some e1')])
    | ESum(c, _, None) ->
        VStruct([(c, None)])

    | EVar(v) ->
        if Primitive.identifier v
        then
          Primitive.lookup v
        else
          Env.lookup (Named v) e

    (* buggy:

    | ESeq(es) ->
        let rec eval_eseq seq ev = match seq with
          | [] -> ev
          | ex::ess ->
              (eval_eseq ess (eval_expr ex ev))
        in
          eval_eseq es e

    *)

    | _ ->
        failwith "Not Implemented"

  in
    eval p (Env.empty ())

