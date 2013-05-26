open AST
open Runtime

type value = Primitive.t Runtime.value

type env = Primitive.t Runtime.venv

(* This is module of the Weak table used for hash-consing. *)
module WeakValues = Weak.Make(struct
  type t = value
  let equal x y = (x = y)
  let hash = Hashtbl.hash
end)

(* The hashtable used for memoization and the weak hashtable used for
   hash-consing *)
let memo, hc_table =
  if !Memo.flag then
    Hashtbl.create 64, WeakValues.create 64
  else
    Hashtbl.create 0, WeakValues.create 0

(* Hashconsing --
    Wrap every value in this function, e.g.:
      before : (VInt 42)
      after  : (hc (VInt 42)) *)
let hc =
  if !Memo.flag
  then WeakValues.merge hc_table
  else fun v -> v

(**
 * Evaluate a program with an environment.
 *
 * @param p the program (list of definitions
 * @param e the environment
 * @return the new environment
 **)
let rec eval p e =
  (* The "accumulator" of the [List.fold_left] call is the current
     environment. *)
  List.fold_left (
    fun e -> function
      | DVal(v) -> eval_vdef v e
      | _       -> e
  ) e p

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
  let e' =
    (* 1- declare empty functions *)
    List.fold_left
      (fun e' -> function (Binding(i, _), _) -> Env.declare i e')
      e l
  in
    (* 2- bind their bodies *)
    List.iter (function
      | (Binding(Named(_) as i, _), body) ->
          Env.define i (eval_expr body e') e'
      | _ -> ()
    ) l; e'

(* evaluate a vdefinition within an environment *)
and eval_vdef v e = match v with
  | Simple(Binding(i, _), ex) ->
      Env.bind i (eval_expr ex e) e
  | MutuallyRecursive(l) ->
      eval_mutually_recursive l e

(* evaluate an expression within an environment *)
and eval_expr exp envt = match exp with

  (* chars *)
  | EChar(c)   -> hc (VChar c)
  (* ints *)
  | EInt(i)    -> hc (VInt i)
  (* strings *)
  | EString(s) -> hc (VString s)

  (* Annotation: (expr:type) *)
  | EAnnot(exp2, _) ->
      eval_expr exp2 envt

  (* Function application: f(x) *)
  | EApp(f, exp) ->
      begin match (eval_expr f envt) with
      | VPrimitive(p) ->
          Primitive.apply p (eval_expr exp envt)

      | VClosure(closure_envt, branchs) ->
          eval_memo_branchs closure_envt branchs (eval_expr exp envt)

      | _ ->
          raise Primitive.InvalidPrimitiveCall
      end

  (* case { patt => expr | ... }. This captures the current environment.
     This cover also if/then/else, which are represented by a case-like
     expression in the AST. *)
  | ECase(_, branchs) ->
      hc (VClosure(envt, branchs))

  (* definition of an expression *)
  | EDef(v, exp2) ->
      eval_expr exp2 (eval_vdef v envt)

  (* function, defined with the 'fun' keyword. *)
  | EFun(Binding(arg, _), exp2) ->
      let arg' = match arg with
      | Named a -> PVar a
      | _       -> POne
      in
      hc (VClosure(envt, [ Branch(arg', exp2) ]))

  (* product constructors *)
  | EProd(_, cl) ->
      let eval_constr = fun
        (c, ex) -> let ex' = begin match ex with
          | Some exx -> Some (eval_expr exx envt)
          | None     -> None
        end in
          (c, ex')
      in
        hc (VStruct(List.sort compare (List.rev_map eval_constr cl)))

  (* sum contructors *)
  | ESum(c, _, e1) -> let e2 = begin match e1 with
    | Some e1' -> Some (eval_expr e1' envt)
    | None     -> None
  end in hc (VStruct([(c, e2)]))

  (* variable *)
  | EVar(v) ->
      (try
        Primitive.lookup v
      with Not_found ->
        Env.lookup (Named v) envt)

  (* sequence of expressions *)
  | ESeq(es) ->
      eval_eseq es envt

(**
 * Evaluate a sequence of expressions.
 *
 * @param exp_seq the list of expressions
 * @param envt the current environment
 **)
and eval_eseq exp_seq envt =
  (* We use List.fold_left even if we don't
     need an accumulator, since we have to iter
     on the list and then return the last result *)
  List.fold_left (fun _ e -> eval_expr e envt) (hc vunit) exp_seq

(**
 * Evaluate a list of branchs using memoization (if Memo.flag is not
 * set, this is a proxy method to [eval_branchs]). If the list have been
 * called on the given expression in the past, it returns its return
 * value without calling the function. If not, it calls the list of
 * branchs on the expression as usual, and store the return value in
 * a global Hashtbl.
 *
 * @branchs a list of branches
 * @expr the expression on which the function is called
 * @ev the current environment
 **)
and eval_memo_branchs ev branchs exp =
  if !Memo.flag then (
    (*
      Using
        try find with Not_found -> compute and memorize
      is faster than
        if mem then find else      compute and memorize

      see: http://stackoverflow.com/a/12161946/735926
     *)
    try
        Hashtbl.find memo (branchs, exp)
    with Not_found ->
      let result = eval_branchs ev branchs exp in
        Hashtbl.add memo (branchs, exp) result;
        result)
  else
    eval_branchs ev branchs exp

(**
 * evaluate a list of branchs, given an expression.
 * @raise No_match if no pattern matches
 **)
and eval_branchs envt branchs exp =
  match branchs with
  | [] -> raise No_match
  | Branch(patt, exp')::branchs' ->
      try
        (* if this branch matches, return the new environment *)
        eval_branch patt exp' exp envt
      with No_match ->
        (* if not, try the next one *)
        eval_branchs envt branchs' exp

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
 * @return a V-expression
 * @raise No_match if the pattern doesn't match
 **)
and eval_branch patt br_exp input_exp envt =
  let envt' = eval_pattern patt input_exp envt in
    eval_expr br_exp envt'

(**
 * Evaluate a pattern sum. It returns a value option.
 * A PSum is something like that :
 *
 *   K[p]  ~  {K  <- v}
 *   constr[subpatt] ~ pconstr <- econstr
 *
 * @param pconstr  constructor identifier of the pattern
 * @param subpatt  the optional pattern in it (something like 'A[p]', where 'A' is
 *                 the constructor identifier and 'p' the pattern)
 * @param econstr  constructor_identifier of the expression
 * @param subvalue the optional value in it (something like 'A[v]')
 * @param envt     the environment
 * @return         an environment if it matches
 * @raise No_match if it doesn't match
 **)
and eval_psum pconstr subpatt econstr subvalue envt =
  (* If the construtor ids don't match, the pattern doesn't match *)
  if pconstr <> econstr then raise No_match
  else
    match subpatt with
    (* sum with sub-pattern *)
    | Some patt ->
        (* if constructor ids match... *)
        begin match subvalue with

        (* ...then if the expression is something like A[x],
           try to match p with x. *)
        | Some v -> eval_pattern patt v envt

        (* ...else if the expression is something like A (and not A[x]),
           don't match *)
        | _ -> raise No_match
        end

    (* sum without sub-pattern
       if the constructor ids match, then the pattern matches *)
    | _ -> envt

(**
 * Evaluate a pattern product. It returns an environment.
 * A PSum is something like that :
 *
 *  {K1 -> p1 ... kn -> pn} ~ {k1 -> v1 ... kn -> vn}
 *  {       pconstrs      } ~ {     econstrs        }
 *
 * @param pconstrs list of constructor identifiers from the pattern and their
 *                 (optional) sub-patterns.
 * @param econstrs list of constructor identifiers from the expression and
 *                 their (optional) value. This list must be already sorted.
 * @param envt     the current environment
 * @raise No_match if it doesn't match
 **)
and eval_pprod pconstrs econstrs envt =
  try
    List.fold_left2 (
      fun envt' (constr1, subpatt1) (constr2, subpatt2) ->
        eval_psum constr1 subpatt1 constr2 subpatt2 envt'
    ) envt (List.sort compare pconstrs) econstrs
  with
    Invalid_argument _ -> raise No_match

(**
 * Evaluate a pattern on an expression. If it matches, it returns an
 * environment where it may have captured a value. For example, the
 * following pattern:
 *
 *      B[x] => x + 2
 *
 * Captures the sub-value of a constructor 'B' as 'x', and return this
 * environment. Then, when the expression 'x+2' will be evaluated, 'x'
 * will be bound to the captured value.
 *
 * This raises No_match if the pattern doesn't match.
 **)
and eval_pattern patt exp envt =
  match patt with

  (* | A[p] => ... : sum pattern *)
  | PSum(c, _, p ) ->
      begin match exp with
      (* If the given expression is a sum *)
      | VStruct [(c',v)] -> eval_psum c p c' v envt
      (* If the given expression is not a sum *)
      | _ -> raise No_match
      end

  (* | { ... , C -> P } => ... : product pattern *)
  | PProd(_, px) ->
      begin match exp with
      (* If the given expression is a prod *)
      | VStruct ex_li -> eval_pprod px ex_li envt
      (* If the given expression is not a prod *)
      | _ -> raise No_match
      end

  (* | p1 and p2 => ... : 'and' pattern *)
  | PAnd(p1, p2) -> eval_pattern p2 exp (eval_pattern p1 exp envt)

  (* | p1 or p2 => ... : 'or' pattern *)
  | POr(p1, p2) ->
      (try
        eval_pattern p1 exp envt
      with No_match ->
        eval_pattern p2 exp envt)

  (* | not p => ... : this pattern matches only if its sub-pattern
                      doesn't match. *)
  | PNot(p) ->
      (*
        This is a hack to simulate something like this:

            try
                eval_pattern p exp envt
            with No_match -> envt
            else_if_no_exception raise No_match
       *)
      let r =
        (try
          ignore (eval_pattern p exp envt); true
        with No_match -> false)
      in
        if r then raise No_match else envt

  (* | x => ... : set x to the input expression
                  and return the new environment *)
  | PVar(v) -> (Env.bind (Named v) exp envt)

  (* | _ => ... : always matches *)
  | POne -> envt

  (* | 0 => ... : never matches *)
  | PZero -> raise No_match

(**
 * Evaluate a program.
 *
 * @param p the program (list of definitions)
 * @return an environment
 **)
let program p =
  eval p (Env.empty ())

