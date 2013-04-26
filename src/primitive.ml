open AST
open Operator
open Runtime

type t = 
| PRef  of (t value) ref
| PCode of (t value -> t value)

let ( --> ) x y = 
  match x with 
  | AST.EVar x -> (x, y)
  | _ -> assert false

exception InvalidPrimitiveCall 

let int_int_int f x y = 
  match x, y with
  | VInt x, VInt y -> VInt (f x y)
  | _ -> raise InvalidPrimitiveCall

let ctrue = CIdentifier "True"

let cfalse = CIdentifier "False"

let as_bool = function
  | VStruct [ (k, None) ] when k = ctrue ->
    true
  | VStruct [ (k, None) ] when k = cfalse ->
    false
  | v -> 
    raise InvalidPrimitiveCall

let of_bool = function
  | true  -> VStruct [ (ctrue, None) ]
  | false -> VStruct [ (cfalse, None) ]

let bool_bool_bool f x y = 
  of_bool (f (as_bool x) (as_bool y))

let int_int_bool f x y = 
  match x, y with
  | VInt x, VInt y -> of_bool (f x y)
  | _ -> raise InvalidPrimitiveCall

let unary_operator f = 
  VPrimitive (PCode (fun x -> f x))

let binary_operator f =
  unary_operator (fun x -> unary_operator (fun y -> f x y))

let coloneq =  EVar (Identifier ":=") 
let andand = EVar (Identifier "&&") 
let pipepipe = EVar (Identifier "||") 
let le = EVar (Identifier "<=") 
let ge = EVar (Identifier ">=") 
let lt = EVar (Identifier "<") 
let gt = EVar (Identifier ">") 
let bangeq = EVar (Identifier "!=") 

let pnot b = of_bool (not (as_bool b))

let pnegate x = 
  match x with
  | VInt y -> VInt (- y)
  | _ -> raise InvalidPrimitiveCall

let rec generic_equality v1 v2 =
  match v1, v2 with
  | VStruct s1, VStruct s2 ->
    of_bool 
      (List.for_all2 
	 (fun (k1, v1) (k2, v2) -> k1 = k2 && generic_equality' v1 v2) 
	 (canonicalize s1) 
	 (canonicalize s2))
  | VPrimitive (PRef r), v | v, VPrimitive (PRef r) -> 
    generic_equality !r v
  | VPrimitive (PCode _), _ | _, VPrimitive (PCode _) ->
    of_bool false
  | v1, v2 -> 
    of_bool (v1 = v2)

and generic_equality' v1 v2 = 
  match v1, v2 with
  | None, None -> true
  | Some v1, Some v2 -> as_bool (generic_equality v1 v2)
  | _ -> false

let generic_disequality v1 v2 = pnot (generic_equality v1 v2)

let generic_assignment v1 v2 = 
  match v1 with
  | VPrimitive (PRef r) -> r := v2; vunit
  | _ -> raise InvalidPrimitiveCall

let mk_prim, is_prim = 
  let ps = Hashtbl.create 13 in
  (fun id code -> 
    let id = EVar (Identifier id) in 
    Hashtbl.add ps id (); 
    (id, VPrimitive (PCode code))
  ),
  (fun id -> Hashtbl.mem ps id)

let alloc, alloc_code = mk_prim "alloc" (
  fun v -> VPrimitive (PRef (ref v))
)

let read, read_code = mk_prim "read" (
  function 
  | VPrimitive (PRef v) -> !v 
  | _ -> raise InvalidPrimitiveCall
)

let lookup x = List.assoc x [
  minus       --> binary_operator (int_int_int ( - ));
  plus        --> binary_operator (int_int_int ( + ));
  star        --> binary_operator (int_int_int ( * ));
  slash       --> binary_operator (int_int_int ( / ));    
  percent     --> binary_operator (int_int_int ( mod ));
  eq          --> binary_operator generic_equality;
  bangeq      --> binary_operator generic_disequality;
  coloneq     --> binary_operator generic_assignment;
  andand      --> binary_operator (bool_bool_bool ( && ));
  pipepipe    --> binary_operator (bool_bool_bool ( || ));
  le          --> binary_operator (int_int_bool ( <= ));
  ge          --> binary_operator (int_int_bool ( >= ));
  lt          --> binary_operator (int_int_bool ( < ));
  gt          --> binary_operator (int_int_bool ( > ));
  negate      --> unary_operator pnegate;
  boolean_not --> unary_operator pnot;  
  alloc       --> alloc_code;
  read        --> read_code;
]

let apply p v = 
  match p with
  | PCode f -> f v
  | PRef _ -> raise InvalidPrimitiveCall

let identifier p = 
  let p = AST.EVar p in
  is_binop p || is_unop p || is_prim p
    
