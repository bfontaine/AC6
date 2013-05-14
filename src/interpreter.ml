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

  (**
   * Evaluate some mutually recursive function definitions, and
   * return a new environment.
   *
   * @return a new environment with the functions in it
   * @param l a list of mutually recursive definitions
   * @param e the current environment
   **)
  and eval_mutually_recursive l e =
    (* Since some function bodies need to know about some other
       functions, we make it in two steps:
       - first, declare empty functions in the environment
       - then, define their body *)
    let rec fill_env_with_empty_defs l e = match l with
      | []     -> e
      | (Binding(i, _), body)::lx ->
          let e2 = Env.declare i e in
            eval_mutually_recursive lx e2

    and bind_bodies l e = match l with
      | [] -> e
      | (Binding(i, _), body)::lx -> begin match i with
        | Named(i') ->
            Env.define i (eval_expr body e) e;
              bind_bodies lx e
        | Unnamed ->
            bind_bodies lx e
        end
    
    in
      bind_bodies l (fill_env_with_empty_defs l e)
            

  (* evaluate a vdefinition within an environment *)
  and eval_vdef v e = match v with
    | Simple(Binding(i, _), ex) ->
        Env.bind i (eval_expr ex e) e
    | MutuallyRecursive(l) ->
        eval_mutually_recursive l e

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
   *  envt : the environment
   * *)
  and eval_branch patt br_exp input_exp envt = 
    match eval_pattern patt input_exp envt with
    | None       -> None 
    | Some envt' -> Some (eval_expr br_exp envt')

  and eval_pattern patt exp envt =
    match patt with

    (* | A[p] => ... : sum with sub-pattern *)
    | PSum(c, _, Some p) -> begin match exp with

        (* If the given expression is a sum... *)
        | VStruct [(c', v)] ->
            (* ...and if constructor ids match... *)
            if c' = c then begin match v with

            (* ...then if the expression is something like A,
               don't match. *)
            | None    -> None

            (* ...else if the expression is something like A[x],
               try to match p with x. *)
            | Some v' -> eval_pattern p v' envt

            (* If constructor ids don't match, the pattern doesn't match *)
            end else None

        (* If the given expression is not a sum, don't match *)
        | _ -> None
        end

    (* | A => ... : sum without sub-pattern *)
    | PSum(c, _, None) -> begin match exp with

      (* If the given expression is a sum... *)
      | VStruct [(c', _)] ->
          (* ...and the constructor ids match, then the pattern matches *) 
          if c = c' then Some envt
          
          (* else, it doesn't match. *)
          else None

      (* If the given expression is not a sum, don't match *)
      | _ -> None
      end

    | PProd(_, px)  -> failwith "PProd not implemented"

    (* | p1 and p2 => ... *)
    | PAnd(p1, p2)  -> begin match (eval_pattern p1 exp envt) with
      | None       -> None
      | Some envt2 -> eval_pattern p2 exp envt2
      end

    (* | p1 or p2 => ... *)
    | POr(p1, p2) -> begin match (eval_pattern p1 exp envt) with
      | None -> eval_pattern p2 exp envt
      | ve   -> ve
      end

    (* | not p => ... : this pattern matches only if its sub-pattern
     * doesn't match. *)
    | PNot(p) -> begin match eval_pattern p exp envt with
      | Some _ -> None
      | None   -> Some envt
      end
    
    (* | 0 => ... : never matches *)
    | PZero -> None

    (* | x => ... : set x to the input and return the new environment *)
    | PVar(v) -> Some (Env.bind (Named v) exp envt)

    (* | _ => ... : always matches *)
    | POne -> Some envt
  in
    eval p (Env.empty ())

