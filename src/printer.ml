open AST
open Pprint

let ( ++ ) d1 d2 = d1 ^^ space ^^ d2

let ( ^+ ) d1 d2 = d1 ^^ break1 ^^ d2

let paren e = group (text "(" ^^ e ^^ text ")")

let sparen e = group (text "<" ^^ e ^^ text ">")

let maybe f = function
  | None -> empty
  | Some x -> break1 ^^ f x

let may_paren decision printer e = 
  if decision e then paren (printer e) else printer e

let rec destruct_fun = function
  | EFun (b, e) ->
    let bs, e, ty = destruct_fun e in
    (b :: bs, e, ty)
  | EAnnot (e, ty) ->
    ([], e, Some ty)
  | e -> 
    ([], e, None)

let rec program ds = 
  sepmap break0 definition ds ^^ break0

and definition = function
  | DType (tid, params, ty) ->
    kw "type" 
    ++ group (type_identifier tid ^^ type_parameters params ++ sym "=")
    ^+ indent 2 (group (typ ty))
  | DVal v ->
    vdefinition v

and type_parameters = function
  | [] -> empty
  | ts -> break1 ^^ sparen (sepmap (comma ^^ break1) type_identifier ts)
      
and typ = function
  | TVar (tid, args) ->
    type_identifier tid ^^ type_arguments args
  | TArrow (tin, tout) as ctx -> 
    group (may_paren_typ ctx tin) 
    ^+ group (sym "->" ++ typ tout)
  | TSum [] ->
    text "{}"
  | TSum ctys ->
    sym "{" ++ sepmap (break1 ^^ sym "+" ^^ space) constructor_type ctys ++ sym "}"
  | TProd ctys ->
    sym "{" ++ sepmap (break1 ^^ sym "*" ^^ space) constructor_type ctys ++ sym "}"
  | TRec (x, ty) ->
    group (kw "rec" ++ type_identifier x ++ kw "is") 
    ++ typ ty

and constructor_type (TConstructor (cid, ty)) = 
  match ty with
  | Some ty ->
    group (constructor_identifier cid ++ group (typ ty))
  | None ->
    constructor_identifier cid

and decide_paren_typ ctx ty = 
  match ctx, ty with
  | TArrow _, (TArrow _ | TRec _) -> true
  | _ -> false

and may_paren_typ ctx tint = 
  may_paren (decide_paren_typ ctx) typ tint

and type_arguments = function
  | [] -> empty
  | ts -> break1 ^^ paren (sepmap (comma ^^ break1) typ ts)
 
and type_identifier (TIdentifier x) = text x

and constructor_identifier (CIdentifier x) = text x

and vdefinition = function
  | Simple (b, e) ->
    kw "val" ++ simplevdef (b, e)
  | MutuallyRecursive ds ->
    kw "def" ++ sepmap (break1 ^^ kw "with" ^^ space) simplevdef ds

and simplevdef (b, e) =
  let bs, e, ty = destruct_fun e in
  nest 2 (group (binding b ++ pbindings bs ++ type_ascription ty ++ sym "=") 
	  ^+ (group (expr e)))

and pbindings bs = 
  sepmap space pbinding bs

and pbinding b = 
  paren (binding b)

and type_ascription = function
  | None -> empty 
  | Some ty -> break1 ^^ sym ":" ++ typ ty

and binding (Binding (x, ty)) = 
  argument_identifier x ^^ type_ascription ty

and argument_identifier = function
  | Unnamed -> text "_"
  | Named v -> value_identifier v

and expr = function
  | EInt x -> 
    text (string_of_int x)
  | EChar c -> 
    sym "'" ^^ text (String.escaped (String.make 1 c)) ^^ sym "'"
  | EString s -> 
    sym "\"" ^^ text (String.escaped s) ^^ sym "\""
  | EVar x -> 
    value_identifier x
  | ESum (c, oty, oe) ->
    group (constructor_identifier c 
	   ^^ maybe typ oty 
	   ^^ maybe bexpr oe)
  | EProd (oty, cps) ->
    group (sym "{" ^^ maybe typ oty ++ components cps ++ sym "}")
  | EAnnot (e, ty) ->
    paren (expr e ++ sym ":" ++ typ ty)
  | EDef (d, e) ->
    vdefinition d ^+ kw "in" ^+ (expr e)
  | ESeq es ->
    sepmap (sym ";" ^^ break1) expr es
  | EApp (e1, e2) as ctx ->
    group (may_paren_expr ctx e1 ^+ may_paren_expr ctx e2)
  | ECase (ty, bs) ->
    sym "{" ++ maybe typ ty ++ sepmap (sym "|" ^^ break1) branch bs ++ sym "}"
  | EFun (b, e) ->
    kw "fun" ++ binding b ++ sym "=>" ^+ expr e

and decide_paren_expr ctx e = 
  match ctx, e with
  | EApp _, (EInt _ | EVar _ | EChar _ | EString _) -> false
  | EApp _, _ -> true
  | _ -> false

and may_paren_expr ctx e = 
  may_paren (decide_paren_expr ctx) expr e

and branch (Branch (p, e)) =
  pattern p ^^ sym "=>" ^^ expr e

and pattern = function
  | PSum  (k, ty, p) -> 
    constructor_identifier k ^^ maybe stype ty 
    ^^ begin match p with
    | None -> empty
    | Some p -> break1 ^^ sym "[" ++ pattern p ++ sym "]"
    end
  | PProd (ty, ds) ->
    maybe stype ty 
    ^^ break1 ^^ sym "{" 
    ++ sepmap (sym "," ^^ break1) psetconstructor ds
    ++ sym "}"
  | PAnd (p1, p2) as ctx ->
    may_paren_pattern ctx p1 
    ++ kw "and" 
    ++ may_paren_pattern ctx p2
  | POr (p1, p2) as ctx ->
    may_paren_pattern ctx p1 
    ++ kw "or" 
    ++ may_paren_pattern ctx p2
  | PNot p as ctx ->
    kw "not" ++ may_paren_pattern ctx p
  | PZero ->
    sym "0"
  | PVar x ->
    value_identifier x
  | POne ->
    sym "_"

and decide_paren_pattern ctx p = 
  match ctx, p with
  | PAnd _, POr _ -> true
  | PNot _, (PVar _ | PZero | POne) -> false
  | PNot _, _ -> true
  | _ -> false

and may_paren_pattern ctx p =
  may_paren (decide_paren_pattern ctx) pattern p

and psetconstructor (k, p) =
  constructor_identifier k ++ sym "<-" ++ pattern p

and stype ty = 
  kw "at" ++ typ ty

and components cps = 
  sepmap (semicolon ^^ break1) component cps

and component (cid, e) =
  match e with 
  | None -> 
    constructor_identifier cid
  | Some e -> 
    group (constructor_identifier cid ++ sym "<-" ++ 
	     nest 2 (group (expr e)))

and bexpr e = sym "[" ^+ expr e ^+ sym "]"

and value_identifier (Identifier x) = text x

and sym x = text x

and kw x  = text x

and semicolon = text ";"


