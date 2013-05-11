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

          | VClosure(e', branchs) ->
              eval_branchs e' branchs e2
          end

    (* case (and if/then/else) *)
    | ECase(_, branchs) ->
        VClosure(e, branchs)

    (* chars *)
    | EChar(c)   -> VChar(c)

    | EDef(v, exp2) ->
        eval_expr exp2 (eval_vdef v e)

    (* function *)
    | EFun(Binding(arg, _), exp2) ->
        let arg' = match arg with
        | Named a -> (PVar a)
        | Unnamed -> (POne)
        in
        VClosure(e, [ Branch(arg', exp2) ])

    (* ints *)
    | EInt(i)    -> VInt(i)

    (* product constructors *)
    | EProd(_, cl) ->
        let eval_constr = fun
          (c', ex) -> let ex' = begin match ex with
            | Some exx -> Some (eval_expr exx e)
            | None     -> None
          end in
            (c', ex')
        in
          VStruct(List.map eval_constr cl)

    (* strings *)
    | EString(s) -> VString(s)

    (* sum contructors *)
    | ESum(c, _, e1) -> let e2 = begin match e1 with
      | Some e1' -> (Some (eval_expr e1' e))
      | None -> None
    end in VStruct([(c, e2)])

    | EVar(v) ->
        if Primitive.identifier v
        then
          Primitive.lookup v
        else
          Env.lookup (Named v) e

    | ESeq(es) -> match es with
      (* This should not happen, since sequences of expressions contain 2+
       * expressions. *)
      | [] -> vunit
      | [ex] ->
          eval_expr ex e

      | ex::es'
          -> let _ = eval_expr ex e in
             eval_expr (ESeq es') e

  (* evaluate a list of branchs, given an expression *)
  and eval_branchs ev branchs exp =
    match branchs with
    | [] -> vunit
    | Branch(p, exp')::branchs' ->
        begin match (eval_branch p exp' exp ev) with
        | Some ve -> ve
        | None -> eval_branchs ev branchs' exp
        end

  (* Evaluate a branch. It returns a value option. A branch is something like
   * that :
   *
   *    pattern => expr
   *
   *  patt : the pattern
   *  br_exp : the expression in the branch
   *  input_exp : the expression on which the branch is 'applied'
   *  ev : the environment
   * *)
  and eval_branch patt br_exp input_exp ev =
    match patt with
    | PSum(c, _, p) -> failwith "PSum not implemented"
    | PProd(_, px)  -> failwith "PProd not implemented"
    | PAnd(p1, p2)  -> failwith "PAnd not implemented"

    | POr(p1, p2) -> begin match (eval_branch p1 br_exp input_exp ev) with
      | None -> eval_branch p2 br_exp input_exp ev
      | ve   -> ve
      end

    (* If the expression match the pattern (we test it using eval_branch
     * on a simple code (EInt 0), return None. If it doesn't match,
     * evaluate the branch. *)
    | PNot(p) -> begin match eval_branch p (EInt 0) input_exp ev with
      | Some _ -> None
      | None   -> Some (eval_expr br_exp ev)
      end
    
    (* | 0 => ... : never matches *)
    | PZero ->
        None

    (* | x => ... : set x to the input and evaluate the expression
     *              with this new environment *)
    | PVar(v) ->
        Some (eval_expr br_exp (Env.bind (Named v) input_exp ev))

    (* | _ => ... : always matches *)
    | POne ->
        Some (eval_expr br_exp ev)

  in
    eval p (Env.empty ())

