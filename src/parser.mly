%{
  (** This module implements syntactic analysis. *)

  open AST
  open Sugar
  open Operator

  let parse_error = Error.error "during parsing"

  let mk_binop e1 o e2 = EApp(EApp(o, e1), e2)

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

(**
 * Tokens that don't need priority rules:
 * - CONSTR_ID CHAR ID INT STR ZERO
 * - AT CASE DEF DO END IS REC TYPE VAL WITH IF FUN
 * - COLON COMMA PIPE
 * - L_BRACKET L_PAREN R_PAREN R_BRACKET L_SQUARE R_SQUARE
 * - UNDERSC TILDE
 *
 **)

%left BINOP_NEUTRE
%nonassoc COLON_EQ IN REC_TYPE WHERE

%right SEMICOLON
%nonassoc THEN
%nonassoc ELSE

%right R_ARROW DBL_R_ARROW L_ARROW
%right EQ NE LE LT GE GT

%left MINUS
%left PLUS

%left BINOP_INF

%left PERCENT
%left STAR
%left SLASH

%left BINOP_SUP

%left ANDAND
%left PIPEPIPE

%left OR
%left AND
%nonassoc NOT

%right DOT

%nonassoc UNOP

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
    c=CONSTR_ID { CIdentifier(c) }

constr:
  (* aConstr [ type ] *)
    c=constr_id t=typ? { TConstructor(c, t) }

constr_def:
  (* aConstr [ <- expr ] *)
    c=constr_id e=preceded(L_ARROW , expr) { (c, Some e) }

constr_defs:
  (* aConstrDef [ ; aConstrDef ; aConstrDef ; ... ] *)
    l=snl(SEMICOLON, constr_def) { l }

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
    DEF v=var_id bl=bindings t=preceded(COMMA,typ)? EQ e=expr
        { (Binding(Named(v), None), mk_fundef bl t e) }

with_list:
  (* [ with ... with ... ... ] *)
    l=with_st* { l }

with_st:
  (* with aVarId [ (binding) (binding) ... ] : aType = expr *)
    WITH v=var_id bl=bindings t=preceded(COMMA,typ)?
      EQ e=expr { (Binding(Named(v), None), mk_fundef bl t e) }


(** == Expressions == *)

app_expr:
  (* aVarId *)
    v=var_id { EVar(v) }

  (* (expr) *)
  | p=p_delimited(expr) { p }

  (* ( expr : type ) *)
  | L_PAREN e=expr COLON t=typ R_PAREN { EAnnot(e, t) }


expr:
  (* aVDefinition in expr *)
    v=vdefinition IN e=expr             { EDef(v, e)     }
  (* expr where aVDefinition end *)
  | e=expr WHERE v=vdefinition END      { EDef(v, e)     }
   (* expr ; expr *)
  | e1=expr SEMICOLON e2=expr           { ESeq([e1; e2]) }
    (* expr.expr *)
  | e1=expr DOT e2=expr                 { EApp(e2, e1)   }
  (*    expr + expr
     or expr - expr
     or expr * expr
     or expr / expr
     or expr = expr
     or ...         *)
  | e1=expr o=binop_neutre e2=expr %prec BINOP_NEUTRE { mk_binop e1 o e2      }
  | e1=expr o=binop_inf e2=expr %prec BINOP_INF       { mk_binop e1 o e2      }
  | e1=expr o=binop_sup e2=expr %prec BINOP_SUP       { mk_binop e1 o e2      }

   (* -expr *)
  | e=preceded(MINUS, expr_init)        { EApp(negate, e)       }
  (* ~expr *)
  | e=preceded(TILDE, expr) %prec UNOP  { EApp(boolean_not, e)  }

  | e=expr_init | e=expr_constr         { e                     }

litteral:
  (* anInt *)
    i=INT                               { EInt(i)        }
  | ZERO                                { EInt(0)        }

  (* aChar *)
  | c=CHAR                              { EChar(c)       }

  (* aString *)
  | s=STR                               { EString(s)     }

(* these expressions can be in the right part of the 'expr expr' rule *)
expr_init:

    e=litteral | e=app_expr                      { e                     }

  (* case [ at aType ] { [ | ] aBranch [ | aBranch | aBranch | ... ] } *)
  | CASE t=preceded(AT, typ)?
      b=br_delimited(branch_list)                { ECase(t, b)           }

  (* if expr then expr else expr *)
  | IF c=expr THEN e1=expr ELSE e2=expr          { mk_ifthenelse c e1 e2 }

  (* if expr then expr *)
  | IF c=expr THEN e1=expr                       { mk_ifthen c e1        }

  (* fun [ (binding) (binding) ... ] [ : aType ] => expr *)
  | FUN bl=bindings
      t=preceded(COLON, typ)? DBL_R_ARROW e=expr { mk_fundef bl t e         }

  (* do { expr } *)
  | DO e=br_delimited(expr)                      { mk_do e               }

  (* expr expr *)
  | e1=app_expr e2=expr_init                     { EApp(e1, e2)          }


(* sum/product constructor expressions *)
expr_constr:
  (* constr_id [ at aType ] [ \[ expr \] ] *)
    c=constr_id t=preceded(AT, typ)?
      e=sq_delimited(expr)?             { ESum(c, t, e)  }

  (* [ at aType ] { aConstrId <- expr [, aConstrId <- expr, ... ] } *)
  | t=ioption(preceded(AT, typ))
      cl=br_delimited(constr_defs)      { EProd(t, cl)   }    

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

binop_inf:
  (* + *)
    PLUS     { plus     }

  (* - *)
  | MINUS    { minus    }
  
binop_sup:
  (* % *)
   PERCENT  { percent  }
  (* * *)

  | STAR     { star     }
  
  (* / *)
  | SLASH    { slash    }
  
binop_neutre:
  (* = *)
   EQ       { eq       }
  
  (* := *)
  | COLON_EQ { coloneq  }
  
  (* && *)
  | ANDAND   { andand   }
  
  (* || *)
  | PIPEPIPE { pipepipe }
  
  (* <= *)
  | LE       { le       }
  
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
  (* aConstrId [ -> pattern ] [ ; aConstrId [ -> * pattern ] ; ... ] *)
    l=snl(COMMA, constr_pattern) { l }

pattern:
  (* aConstrId [ at aType ] [ \[ pattern \] ] *)
    c=constr_id t=preceded(AT, typ)?
      p=sq_delimited(pattern)?         { PSum(c, t, p) }

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

  (* _ *)
  | UNDERSC                            { POne          }


(** == Types == *)

type_id:
  (* aTypeId *)
    t=ID { TIdentifier(t) }

%inline diples_comma_separated_list(X):
  (* [ < X [, X, X, ... ] > ] *)
    xl = loption(delimited(LT, snl(COMMA, X), GT)) { xl }

typ:
  (* aTypeId [ < aType [ , aType, ... ] > ] *)
    ti=type_id tl=diples_comma_separated_list(typ) { TVar(ti, tl)   }

  (* aType -> aType *)
  | t1=typ R_ARROW t2=typ                          { TArrow(t1, t2) }

  (* { contrId [ aType ] + constrId [ aType ] [ + ... ] } *)
  | p=br_delimited(plus_constr_list)               { TSum(p)        }

  (* { contrId [ aType ] * constrId [ aType ] [ * ... ] } *)
  | p=br_delimited(star_constr_list)               { TProd(p)       }

  (* { constrId [ aType ] } *)
  | p=br_delimited(constr)                         { TSum([p])      }

  (* rec aTypeId is aType *)
  | REC ti=type_id IS t=typ %prec REC_TYPE         { TRec(ti, t)    }

  (* (aType) *)
  | t=p_delimited(typ)                             { t              }

