open AST 
open Runtime

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv

(* The hashtable used for the memoization *)
let memo = Hashtbl.create 42

(**
 * Evaluate a program with an environment.
 *
 * @param p the program (list of definitions
 * @param e the environment
 * @return the new environment
 **)
let rec eval p e = match p with
  (* No definitions *)
  | [] -> e
  (* One or more definitions *)
  | d::defs ->
      (* evaluate the definition, and iter on the
       * rest of the program *)
      let e' = begin match d with
        | DVal(v) -> eval_vdef v e
        | _ -> e (* DType(_): there's no type here *)
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
    | (Binding(Named(_) as i, _), body)::lx ->
        Env.define i (eval_expr body e) e;
          bind_bodies lx e

    | _::lx -> (* Unnamed *)
        bind_bodies lx e
  
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

  (* chars *)
  | EChar(c)   -> VChar(c)
  (* ints *)
  | EInt(i)    -> VInt(i)
  (* strings *)
  | EString(s) -> VString(s)

  (* Annotation: (expr:type) *)
  | EAnnot(exp2, _) ->
      eval_expr exp2 e

  (* Function application: f(x) *)
  | EApp(f, e1) ->
      let fn = eval_expr f e in
        begin match fn with
        | VPrimitive(p) ->
            Primitive.apply p (eval_expr e1 e)

        | VClosure(ev, branchs) ->
            (* choose between eval_branchs and eval_memo_vclosure,
               depending of the memoization flag. *)
            (if !Memo.flag
            then eval_memo_vclosure
            else eval_branchs) ev branchs (eval_expr e1 e)
        
        | VInt(_)
        | VChar(_)
        | VString(_)
        | VStruct(_) ->
            raise Primitive.InvalidPrimitiveCall
        end

  (* case { patt => expr | ... }  (and if/then/else) *)
  | ECase(_, branchs) ->
      VClosure(e, branchs)

  (* definition *)
  | EDef(v, exp2) ->
      eval_expr exp2 (eval_vdef v e)

  (* function *)
  | EFun(Binding(arg, _), exp2) ->
      let arg' = match arg with
      | Named a -> (PVar a)
      | Unnamed -> POne
      in
      VClosure(e, [ Branch(arg', exp2) ])

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

  (* sum contructors *)
  | ESum(c, _, e1) -> let e2 = begin match e1 with
    | Some e1' -> (Some (eval_expr e1' e))
    | None     -> None
  end in VStruct([(c, e2)])

  | EVar(v) ->
      if Primitive.identifier v
      then Primitive.lookup v
      else Env.lookup (Named v) e

  | ESeq(es) -> match es with
    | [] -> vunit
    | [ex] ->
        eval_expr ex e

    | ex::es' ->
        let _ = eval_expr ex e in
           eval_expr (ESeq es') e

(**
 * Evaluate a list of branchs using memoization. If the list have been
 * called on the given expression in the past, it returns its return
 * value without calling the function. If not, it calls the list of
 * branchs on the expression as usual, and store the return value in
 * a global Hashtbl.
 *
 * @branchs a list of branches
 * @expr the expression on which the function is called
 * @ev the current environment
 **)
and eval_memo_vclosure ev branchs expr =
  if Hashtbl.mem memo branchs
  then Hashtbl.find memo branchs
  else
    let result = eval_branchs ev branchs expr in
      Hashtbl.add memo branchs result;
      result

(* evaluate a list of branchs, given an expression *)
and eval_branchs ev branchs exp =
  match branchs with
  | [] -> failwith "This pattern matching didn't match!"
  | Branch(p, exp')::branchs' ->
      begin match (eval_branch p exp' exp ev) with
      (* if this branch matches, return the new environment *)
      | Some ve -> ve
      (* if not, try the next one *)
      | None    -> eval_branchs ev branchs' exp
      end

(**
 * Evaluate a branch. It returns a value option. A branch is something like
 * that :
 *
 *    pattern => expr
 *
 * @param patt the pattern
 * @param br_exp the expression in the branch
 * @param input_exp the expression on which the branch is 'applied'
 * @param envt the environment
 **)
and eval_branch patt br_exp input_exp envt = 
  match eval_pattern patt input_exp envt with
  | None       -> None 
  | Some envt' -> Some (eval_expr br_exp envt')

(**
 * Evaluate a pattern sum. It returns a value option. 
 * A PSum is something like that :
 *
 *   K[p]  ~  {K  <- v}
 *   constr[patt] ~ ex_c <- ex_v
 *
 * @param constr constructor_identifier
 * @param patt   pattern
 * @param ex_c   constructor_identifier with the expression
 * @param ex_v   pattern wiht the expression
 * @param envt   the environment
 **)
and eval_psum constr patt ex_c ex_v envt =
  match patt with
  (* sum with sub-pattern *)
  | Some p -> 
      (* ...and if constructor ids match... *)
      if ex_c = constr then begin match ex_v with

      (* ...then if the expression is something like A, don't match. *)
      | None    -> None

      (* ...else if the expression is something like A[x],
         try to match p with x. *)
      | Some v' -> eval_pattern p v' envt
      (* If constructor ids don't match, the pattern doesn't match *)
       end else None

  (* sum without sub-pattern  *)
  | None ->
    (* ...and the constructor ids match, then the pattern matches *) 
        if constr = ex_c then Some envt
        
        (* if not, it doesn't match. *)
        else None

(**
 * Evaluate a pattern produit. It returns a value option.
 * A PSum is something like that :
 *  
 *  {K1 -> p1 ... kn -> pn} ~ {k1 -> v1 ... kn -> vn}
 *  {         px          } ~ {        ex_li        }
 *
 * @param px    list of contructor_identifier and pattern
 * @param ex_li list of construcor_identifier and value
 * @param envt  the environment
 **)
and eval_pprod px ex_li envt = 
  match (px,ex_li) with 
  |((c,p)::px' , (c',p')::ex_li' ) -> let envt' = eval_psum c p c' p' envt in 
      begin match envt' with
      | Some envt'' -> eval_pprod px' ex_li' envt''
      | None -> None
      end
  |([],[]) -> Some envt
  |(_,_)   -> None


and eval_pattern patt exp envt =
  match patt with

  (* | A[p] => ... *)
  | PSum(c, _,p ) -> 
      begin match exp with 
      (* If the given expression is a sum ...*)
      | VStruct [(c',v)] -> eval_psum c p c' v envt
      (* If the given expression is not a sum, don't match *)
      | _ -> None
      end 
  (*  { , C -> P } => ...    *)
  | PProd(_, px)  -> 
      begin match exp with 
      (* If the given expression is a prod ...*)
      | VStruct ex_li -> eval_pprod px ex_li envt
      (* If the given expression is not a prod, don't match *)
      | _ -> None
      end

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

  (* | x => ... : set x to the input and return the new environment *)
  | PVar(v) -> Some (Env.bind (Named v) exp envt)

  (* | _ => ... : always matches *)
  | POne -> Some envt
  
  (* | 0 => ... : never matches *)
  | PZero -> None

(**
 * Evaluate a program.
 *
 * @param p the program (list of definitions)
 * @return an environment
 **)
let program p =
  eval p (Env.empty ())

