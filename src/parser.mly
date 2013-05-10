%{
  (** This module implements syntactic analysis. *)

  open AST
  open Sugar
  open Operator

  let parse_error = Error.error "during parsing"

  (* make a binop *)
  let mk_binop e1 o e2 = EApp(EApp(o, e1), e2)

  (* make an ESeq given two expressions *)
  let mk_seq_expr e1 e2 = match e1, e2 with
    | ESeq(l1), ESeq(l2) -> ESeq(l1@l2)
    | ESeq(l1), _        -> ESeq(l1@[e2])
    | _       , ESeq(l2) -> ESeq(e1::l2)
    | _       , _        -> ESeq([e1; e2])

%}


%token EOF
%token <int> INT
%token <char> CHAR
%token <string> STR
%token <string> ID
%token <string> CONSTR_ID
%token PIPE L_PAREN R_PAREN L_BRACKET R_BRACKET L_SQUARE R_SQUARE
%token PLUS STAR EQ MINUS SLASH PERCENT COLON_EQ
%token ANDAND PIPEPIPE LE GE NE LT GT
%token TILDE COLON SEMICOLON DOT COMMA UNDERSC ZERO
%token DBL_R_ARROW R_ARROW L_ARROW
%token IF ELSE THEN FUN DO CASE DEF WITH AT IN WHERE END
%token VAL IS TYPE REC OR AND NOT


%start<AST.program> program


(* e=e, e:=e, e&&e, e||e *)
%left BINOP_NO_PRIORITY

(* :=, in, "rec ... is ...", where *)
%nonassoc COLON_EQ IN REC_TYPE 
%nonassoc WHERE

(* then, else *)
%nonassoc THEN
%nonassoc ELSE

(* ->, => *)
%right R_ARROW DBL_R_ARROW

(* = *)
%right EQ

(* -, + *)
%left MINUS
%left PLUS

(* e+e, e-e *)
%left BINOP_LOW_PRIORITY

(* %, *, / *)
%left PERCENT
%left STAR
%left SLASH

(* e%e, e*e, e/e *)
%left BINOP_HIGH_PRIORITY

(* &&, || *)
%left ANDAND
%left PIPEPIPE

(* or, and, not *)
%left OR
%left AND
%nonassoc NOT

(* . (rule expr.expr) *)
%right DOT

%%

(* shortcuts *)
%inline snl(S, X):
    l=separated_nonempty_list(S, X) { l }

%inline  p_delimited(X): d=delimited(L_PAREN,   X, R_PAREN)   { d }
%inline br_delimited(X): d=delimited(L_BRACKET, X, R_BRACKET) { d }
%inline sq_delimited(X): d=delimited(L_SQUARE,  X, R_SQUARE)  { d }


(**
 * = Syntax of comments =
 *
 * [ e ] : optional 'e'
 * \[ \] : litteral square brackets
 **)

(**
 * a program is a list of definitions
 *)
program:
  (* [ aDefinition aDefinition ... ] *)
    p=definition* EOF { p }


(** == Bindings == *)

binding:
  (* argId [ : aType ] *)
    a=argument_identifier t=preceded(COLON, typ)? { Binding(a, t) }

bindings:
  (* [ (binding) (binding) (binding) ... ] *)
    l=p_delimited(binding)* { l }


(** == Branches = *)

branch_list:
  (* [ | ] aBranch [ | aBranch | aBranch | ... ] *)
    PIPE? l=snl(PIPE, branch) { l }

branch:
  (* aPattern => expr *)
    p=pattern DBL_R_ARROW e=expr { Branch(p, e) }


(** == Constructors == *)

constr_id:
  (* aConstrId *)
    c=constr_id_no_underscore { c }
  (* _ *)
  | UNDERSC    { CIdentifier("_") }

constr_id_no_underscore:
    c=CONSTR_ID { CIdentifier(c) }

constr:
  (* aConstr [ type ] *)
    c=constr_id t=typ? { TConstructor(c, t) }

constr_def:
  (* aConstr [ <- expr ] *)
    c=constr_id e=preceded(L_ARROW , expr)? { (c, e) }

constr_defs:
  (* aConstrDef [ ; aConstrDef ; aConstrDef ; ... ] *)
    l=snl(COMMA, constr_def) { l }

plus_constr_list:
  (* aConstr + aConstr [ + aConstr + aConstr ... ] *)
    c=constr PLUS l=snl(PLUS, constr) { c::l }

star_constr_list:
  (* aConstr * aConstr [ * aConstr * aConstr ... ] *)
    c=constr STAR l=snl(STAR, constr) { c::l }


(** == Definitions == *)

definition:
  (* type aTypeId [ < aTypeId [ , aTypeId, ... ] > ] = aType *)
    TYPE t1=type_id tl=diples_comma_separated_list(type_id)
      EQ t2=typ { DType(t1, tl, t2) }

  (* aVDefinition *)
  | v=vdefinition { DVal(v) }

vdefinition:
  (* val aBinding = expr *) 
    VAL b=binding EQ e=expr { Simple(b, e) }

  (* def aVarId [ (binding) (binding) ... ] : aType = expr [ with ... with ... ] *)
  | v=simple_vdef w=with_list { MutuallyRecursive(v::w) }

(* this is just a vdefinition without 'with' statements *)
simple_vdef:
  (* def aVarId [ (binding) (binding) ... ] : aType = expr *)
    DEF v=var_id bl=bindings t=preceded(COLON,typ)? EQ e=expr
        { (Binding(Named(v), None), mk_fundef bl t e) }

with_list:
  (* [ with ... with ... ... ] *)
    l=with_st* { l }

with_st:
  (* with aVarId [ (binding) (binding) ... ] : aType = expr *)
    WITH v=var_id bl=bindings t=preceded(COLON,typ)?
      EQ e=expr { (Binding(Named(v), None), mk_fundef bl t e) }


(** == Expressions == *)

unop_expr:
   (* -expr *)
    e=preceded(MINUS, unopable_expr) { EApp(negate, e)       }
  (* ~expr *)
  | e=preceded(TILDE, unopable_expr) { EApp(boolean_not, e)  }

unopable_expr:
    e1=app_expr_right             { e1           }
  (* expr expr *)
  | e1=app_expr e2=app_expr_right { EApp(e1, e2) }


(* comparable expressions, i.e. can appear before <, >, =, !=, etc *)
comparable_expr: e=safe_expr | e=litteral  { e }


(* These expressions are safe, i.e. they can't create any priority problem *)
safe_expr:
  (* aVarId *)
    v=var_id { EVar(v) }

  (* (expr) *)
  | p=p_delimited(expr) { p }

  (* ( expr : type ) *)
  | L_PAREN e=expr_alone COLON t=typ R_PAREN { EAnnot(e, t) }

safe_expr_or_litteral:
    e=safe_expr | e=litteral { e }

(* applicable expressions *)
app_expr:
    a=safe_expr { a }
  | a1=app_expr a2=safe_expr_or_litteral { EApp(a1,a2) }

(* an expression can be... *)
expr:
  (* a sequence of expressions, or *)
    e=seq_expr
  (* a standalone expression. *)
  | e=expr_alone { e }

seq_expr:
  (* expr; expr [; expr; expr ... ] *)
    e1=expr_alone SEMICOLON e2=expr_alone { mk_seq_expr e1 e2 }
  | e=expr_alone  SEMICOLON s=seq_expr    { mk_seq_expr e  s }

expr_alone:
  (* aVDefinition in expr *)
    v=vdefinition IN e=expr_alone        { EDef(v, e)     }
  (* expr where aVDefinition end *)
  | e=expr_alone WHERE v=vdefinition END { EDef(v, e)     }

  (* expr.expr *)
  | e1=expr_alone DOT e2=expr_alone      { EApp(e2, e1)   }

  (*    expr <  expr
     or expr <= expr
     or expr >  expr
     or expr >= expr
     or expr =  expr *)
  | e1=comparable_expr
      o=comparison_binop
        e2=comparable_expr

  (*    expr + expr
     or expr - expr
     or expr * expr
     or expr / expr
     or expr % expr *)
  | e1=expr_alone
      o=binop_no_priority 
        e2=expr_alone %prec BINOP_NO_PRIORITY
  | e1=expr_alone
      o=binop_low_priority
        e2=expr_alone %prec BINOP_LOW_PRIORITY
  | e1=expr_alone
      o=binop_high_priority
        e2=expr_alone %prec BINOP_HIGH_PRIORITY { mk_binop e1 o e2    }

  | e=unop_expr
  | e=unopable_expr
  | e=expr_constr { e }

litteral:
  (* anInt *)
    i=INT  { EInt(i)    }
  | ZERO   { EInt(0)    }

  (* aChar *)
  | c=CHAR { EChar(c)   }

  (* aString *)
  | s=STR  { EString(s) }

app_expr_right:
    e=litteral
  | e=safe_expr                             { e                     }

  (* case [ at aType ] { [ | ] aBranch [ | aBranch | aBranch | ... ] } *)
  | CASE t=preceded(AT, typ)?
      b=br_delimited(branch_list)           { ECase(t, b)           }

  (* if expr then expr else expr *)
  | IF c=expr_alone
      THEN e1=expr_alone ELSE e2=expr_alone { mk_ifthenelse c e1 e2 }

  (* if expr then expr *)
  | IF c=expr_alone THEN e1=expr_alone      { mk_ifthen c e1        }

  (* fun [ (binding) (binding) ... ] [ : aType ] => expr *)
  | FUN bl=bindings
      t=preceded(COLON, typ)?
        DBL_R_ARROW e=expr_alone            { mk_fundef bl t e      }

  (* do { expr } *)
  | DO e=br_delimited(expr)                 { mk_do e               }


(* sum/product constructor expressions *)
expr_constr:
  (* constr_id [ at aType ] [ \[ expr \] ] *)
    c=constr_id t=preceded(AT, typ)?
      e=sq_delimited(expr)?          { ESum(c, t, e) }

  (* [ at aType ] { aConstrId <- expr [, aConstrId <- expr, ... ] } *)
  | t=ioption(preceded(AT, typ))
      cl=br_delimited(constr_defs)   { EProd(t, cl)  }    

(** == Identifiers == *)

argument_identifier:
  (* aVarId *) 
    v=var_id { Named(v) }

  (* _ *)
  | UNDERSC  { Unnamed  }

var_id:
  (* aVarId *)
    v=ID { Identifier(v)  }

(** == Operations == *)

(** === Binary Operations === *)

binop_low_priority:
  (* + *)
    PLUS     { plus     }

  (* - *)
  | MINUS    { minus    }
  
binop_high_priority:
  (* % *)
    PERCENT  { percent  }
  (* * *)

  | STAR     { star     }
  
  (* / *)
  | SLASH    { slash    }
  
binop_no_priority:
  (* = *)
    EQ       { eq       }
  
  (* := *)
  | COLON_EQ { coloneq  }
  
  (* && *)
  | ANDAND   { andand   }
  
  (* || *)
  | PIPEPIPE { pipepipe }

comparison_binop:
  (* <= *)
    LE       { le       }
  
  (* >= *)
  | GE       { ge       }
  
  (* < *)
  | LT       { lt       }
  
  (* > *)
  | GT       { gt       }
  
  (* != *)
  | NE       { bangeq   }

(** === Unary Operations === *)

(** == Patterns == *)

constr_pattern:
  (* aConstrId [ -> pattern ] *)
    cp=pair(constr_id, preceded(R_ARROW, pattern)?) { cp }

constr_patterns:
  (* aConstrId [ -> pattern ] [ ; aConstrId [ -> pattern ] ; ... ] *)
    l=snl(COMMA, constr_pattern) { l }

pattern:
  (* aConstrId [ at aType ] [ \[ pattern \] ] *)
    c=constr_id_no_underscore t=preceded(AT, typ)?
      p=sq_delimited(pattern)?         { PSum(c, t, p) }

  (* '_' is a valid constr_id *)
  | UNDERSC t=preceded(AT, typ)?
      p=sq_delimited(pattern)? { match (t, p) with
                                 | (None, None) -> POne
                                 | _ -> PSum(CIdentifier("_"), t, p)
                               }
  (* [ at aType ] { aConstrId [ -> pattern ] [ ; aConstrId [ -> pattern ] ; ... ] } *)
  | t=preceded(AT, typ)?
      cp=br_delimited(constr_patterns) { PProd(t, cp)  }

  (* pattern or pattern *)
  | p1=pattern OR p2=pattern           { POr(p1, p2)   }

  (* pattern and pattern *)
  | p1=pattern AND p2=pattern          { PAnd(p1, p2)  }

  (* not pattern *)
  | NOT p=pattern                      { PNot(p)       }

  (* (pattern) *)
  | p=p_delimited(pattern)             { p             }

  (* aVarId *)
  | v=var_id                           { PVar(v)       }

  (* 0 *)
  | ZERO                               { PZero         }


(** == Types == *)

type_id:
  (* aTypeId *)
    t=ID { TIdentifier(t) }

%inline diples_comma_separated_list(X):
  (* [ < X [, X, X, ... ] > ] *)
    xl=loption(delimited(LT, snl(COMMA, X), GT)) { xl }

typ:
  (* aTypeId [ < aType [ , aType, ... ] > ] *)
    ti=type_id tl=diples_comma_separated_list(typ) { TVar(ti, tl)   }

  (* aType -> aType *)
  | t1=typ R_ARROW t2=typ                          { TArrow(t1, t2) }

  (* { contrId [ aType ] + constrId [ aType ] [ + ... ] } *)
  | p=br_delimited(plus_constr_list)               { TSum(p)        }

  (* { contrId [ aType ] * constrId [ aType ] [ * ... ] } *)
  | p=br_delimited(star_constr_list)               { TProd(p)       }

  (* {} *)
  | L_BRACKET R_BRACKET                            { TSum([])       }

  (* { constrId [ aType ] } *)
  | p=br_delimited(constr)                         { TSum([p])      }

  (* rec aTypeId is aType *)
  | REC ti=type_id IS t=typ %prec REC_TYPE         { TRec(ti, t)    }

  (* (aType) *)
  | t=p_delimited(typ)                             { t              }

