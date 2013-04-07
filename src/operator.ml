open AST

let plus = EVar (Identifier "+") 
let minus = EVar (Identifier "-")
let star = EVar (Identifier "*") 
let slash = EVar (Identifier "/") 
let percent = EVar (Identifier "%") 
let eq = EVar (Identifier "=") 
let coloneq =  EVar (Identifier ":=") 
let andand = EVar (Identifier "&&") 
let pipepipe = EVar (Identifier "||") 
let le = EVar (Identifier "<=") 
let ge = EVar (Identifier ">=") 
let lt = EVar (Identifier "<") 
let gt = EVar (Identifier ">") 
let bangeq = EVar (Identifier "!=") 

let binops = 
  [ plus; minus; star; slash; percent; eq; coloneq; andand; pipepipe;
    le; ge; lt; gt; bangeq ]

let is_binop x = List.mem x binops 

let print_binop = function
  | EVar (Identifier x) -> x
  | _ -> assert false

let negate = EVar (Identifier "--")

let boolean_not = EVar (Identifier "~")

let unops = [ negate; boolean_not ]

let is_unop x = List.mem x unops

let print_unop = function
  | EVar (Identifier "--") -> "-"
  | EVar (Identifier "~") -> "~"
  | _ -> assert false
