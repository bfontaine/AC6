%{
  (** This module implements syntactic analysis. *)

  open AST
  open Sugar
  open Operator

  let parse_error = Error.error "during parsing"

  let mk_binop e1 o e2 = EApp(EApp(o, e1), e2)
  let mk_unop o e = EApp(o, e)

%}


%token EOF
%token <int> INT
%token <char> CHAR
%token <string> STR
%token <string> VAR_ID
%token <string> TYPE_ID
%token <string> CONSTR_ID
%token PIPE L_PAREN R_PAREN L_BRACKET R_BRACKET L_SQUARE R_SQUARE
%token PLUS STAR EQ MINUS SLASH PERCENT COLON_EQ
%token ANDAND PIPEPIPE LE GE NE LT GT
%token TILDE COLON SEMICOLON DOT COMMA UNDERSC ZERO
%token DBL_R_ARROW R_ARROW L_ARROW
%token IF ELSE THEN FUN DO CASE DEF WITH AT IN WHERE END
%token VAL IS TYPE REC OR AND NOT

(* identifiers + litterals *)
%nonassoc CONSTR_ID VAR_ID STR CHAR INT

%nonassoc L_PAREN
%nonassoc DEF FUN CASE AT

%right IN

%left OR
%left AND
%nonassoc NOT

%nonassoc IS

%right DBL_R_ARROW
%right R_ARROW
%left L_ARROW
%right SEMICOLON

%left EQ
%nonassoc VAL

%left ASSIGN

%nonassoc DO
%nonassoc IF THEN ELSE
%nonassoc WHERE

(* binary operators *)
%left BINOP

(* highest priority *)
%right EXPR_EXPR
%left EXPR_DOT_EXPR

%nonassoc UMINUS UTILDE

%start<AST.program> input

%%

(**
 * = Syntax of comments =
 *
 * [ e ] : optional 'e'
 * \[ \] : litteral square brackets
 **)

(**
 * a program is a list of definitions
 *)
input:
  (* [ aDefinition aDefinition ... ] *)
    p=list(definition) EOF { p }


(** == Bindings == *)

binding:
  (* argId [ : aType ] *)
    a=argument_identifier t=option(preceded(COLON, typ)) { Binding(a, t) }

bindings:
  (* [ (binding) (binding) (binding) ... ] *)
    l=list(delimited(L_PAREN, binding, R_PAREN)) { l }


(** == Branches = *)

branch_list:
  (* [ | ] aBranch [ | aBranch | aBranch | ... ] *)
    option(PIPE) l=separated_nonempty_list(PIPE, branch) { l }

branch:
  (* aPattern => expr *)
    p=pattern DBL_R_ARROW e=expr { Branch(p, e) }


(** == Constructors == *)

constr_id:
  (* aConstrId *)
    c=CONSTR_ID { CIdentifier(c) }

constr:
  (* aConstr [ type ] *)
    c=constr_id t=option(typ) { TConstructor(c, t) }

constr_def:
  (* aConstr [ <- expr ] *)
    c=constr_id e=preceded(L_ARROW , expr) { (c, Some e) }

constr_defs:
  (* aConstrDef [ ; aConstrDef ; aConstrDef ; ... ] *)
    l=separated_nonempty_list(SEMICOLON, constr_def) { l }

plus_constr_list:
  (* aConstr + aConstr [ + aConstr + aConstr ... ] *)
    c=constr PLUS l=separated_nonempty_list(PLUS, constr) { c::l }

star_constr_list:
  (* aConstr * aConstr [ * aConstr * aConstr ... ] *)
    c=constr STAR l=separated_nonempty_list(STAR, constr) { c::l }


(** == Definitions == *)

definition:
  (* type aTypeId [ < aTypeId [ , aTypeId, ... ] > ] = aType *)
    TYPE t1=type_id tl=delimited_list(LT, type_ids, GT)
      EQ t2=typ { DType(t1, tl, t2) }

  (* aVDefinition *)
  | v=vdefinition { DVal(v) }

vdefinition:
  (* val aBinding = expr *) 
    VAL b=binding EQ e=expr %prec ASSIGN { Simple(b, e) }

  (* def aVarId [ (binding) (binding) ... ] : aType = expr [ with ... with ... ] *)
  | DEF v=var_id bl=bindings COLON t=typ EQ e=expr w=with_list
    { MutuallyRecursive((Binding(Named(v), None), mk_fundef bl (Some t) e)::w) }

with_list:
  (* [ with ... with ... ... ] *)
    l=list(with_st) { l }

with_st:
  (* with aVarId [ (binding) (binding) ... ] : aType = expr *)
    WITH v=var_id bl=bindings COLON t=typ
      EQ e=expr { (Binding(Named(v), None), mk_fundef bl (Some t) e) }


(** == Expressions == *)

expr:
  (* anInt *)
    i=INT                                               { EInt(i)                  }

  (* aChar *)
  | c=CHAR                                              { EChar(c)                 }

  (* aString *)
  | s=STR                                               { EString(s)               }

  (* aVarId *)
  | v=var_id                                            { EVar(v)                  }

  (* constr_id [ at aType ] [ \[ expr \] ] *)
  | c=constr_id t=option(preceded(AT, typ))
      e=option(delimited(L_SQUARE, expr, R_SQUARE))     { ESum(c, t, e)            }

  (* [ at aType ] { aConstrId <- expr [, aConstrId <- expr, ... ] } *)
  | t=option(preceded(AT, typ))
      cl=delimited(L_BRACKET, constr_defs, R_BRACKET)   { EProd(t, cl)             }    

  (* ( expr ) *)
  | e=delimited(L_PAREN, expr, R_PAREN)                 { e                        }

  (* ( expr : type ) *)
  | L_PAREN e=expr COLON t=typ R_PAREN                  { EAnnot(e, t)             }

  (* expr ; expr *)
  | e1=expr SEMICOLON e2=expr                           { ESeq([e1; e2])           }

  (* aVDefinition in expr *)
  | v=vdefinition IN e=expr                             { EDef(v, e)               }

  (* expr where aVDefinition end *)
  | e=expr WHERE v=vdefinition END                      { EDef(v, e)               }

  (* expr.expr *)
  | e1=expr DOT e2=expr %prec EXPR_DOT_EXPR             { EApp(e2, e1)             }

  (*    expr + expr
     or expr - expr
     or expr * expr
     or expr / expr
     or expr = expr
     or ...         *)
  | e1=expr o=binop e2=expr %prec BINOP                 { mk_binop e1 o e2         }

  (* -expr *)
  | e=preceded(MINUS, expr) %prec UMINUS                { EApp(negate, e)          }

  (* ~expr *)
  | e=preceded(TILDE, expr) %prec UTILDE                { EApp(boolean_not, e)     }

  (* case [ at aType ] { [ | ] aBranch [ | aBranch | aBranch | ... ] } *)
  | CASE t=option(preceded(AT, typ))
      b=delimited(L_BRACKET, branch_list, R_BRACKET)    { ECase(t, b)              }

  (* if expr then expr else expr *)
  | IF cond=expr THEN e1=expr ELSE e2=expr              { mk_ifthenelse cond e1 e2 }

  (* if expr then expr *)
  | IF cond=expr THEN e1=expr                           { mk_ifthen cond e1        }

  (* fun [ (binding) (binding) ... ] [ : aType ] => expr *)
  | FUN bl=bindings
      t=option(preceded(COLON, typ)) DBL_R_ARROW e=expr { mk_fun bl t e            }

  (* do { expr } *)
  | DO e=delimited(L_BRACKET, expr, R_BRACKET)          { mk_do e                  }

  (* expr expr *)
  | e1=expr e2=expr %prec EXPR_EXPR                     { EApp(e1, e2)             }


(** == Identifiers == *)

argument_identifier:
  (* aVarId *) 
    v=var_id { Named(v) }

  (* _ *)
  | UNDERSC  { Unnamed  }

var_id:
  (* aVarId *)
    v=VAR_ID       { Identifier(v)  }

(** == Operations == *)

(** === Binary Operations === *)

binop:
  (* + *)
    PLUS     { plus     }

  (* - *)
  | MINUS    { minus    }
  
  (* * *)
  | STAR     { star     }
  
  (* / *)
  | SLASH    { slash    }
  
  (* % *)
  | PERCENT  { percent  }
  
  (* = *)
  | EQ       { eq       }
  
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
    cp=pair(constr_id, option(preceded(R_ARROW, pattern))) { cp }

constr_patterns:
  (* aConstrId [ -> pattern ] [ ; aConstrId [ -> * pattern ] ; ... ] *)
    l=separated_nonempty_list(SEMICOLON, constr_pattern) { l }

pattern:
  (* aConstrId [ at aType ] [ \[ pattern \] ] *)
    c=constr_id t=option(preceded(AT, typ))
      p=option(delimited(L_SQUARE, pattern, R_SQUARE))    { PSum(c, t, p) }

  (* [ at aType ] { aConstrId [ -> pattern ] [ ; aConstrId [ -> pattern ] ; ... ] } *)
  | t=option(preceded(AT, typ))
      cp=delimited(L_BRACKET, constr_patterns, R_BRACKET) { PProd(t, cp)  }

  (* pattern or pattern *)
  | p1=pattern OR p2=pattern                              { POr(p1, p2)   }

  (* pattern and pattern *)
  | p1=pattern AND p2=pattern                             { PAnd(p1, p2)  }

  (* not pattern *)
  | NOT p=pattern                                         { PNot(p)       }

  (* aVarId *)
  | v=var_id                                              { PVar(v)       }

  (* 0 *)
  | ZERO                                                  { PZero         }

  (* _ *)
  | UNDERSC                                               { POne          }


(** == Types == *)

type_id:
  (* aTypeId *)
    t=TYPE_ID     { TIdentifier(t) }

type_ids:
  (* aTypeId [, aTypeId, aTypeId, ... ] *)
    l=separated_nonempty_list(COMMA, type_id) { l }

%inline delimited_list(BEGIN, XLIST, END):
  (* [ BEGIN XLIST END ] *)
    xl = loption(delimited(BEGIN, XLIST, END)) { xl }

typ:
  (* aTypeId [ < aType [ , aType, ... ] > ] *)
    ti=type_id tl=delimited_list(LT, types, GT)         { TVar(ti, tl)   }

  (* aType -> aType *)
  | t1=typ R_ARROW t2=typ                               { TArrow(t1, t2) }

  (* { contrId [ aType ] + constrId [ aType ] [ + ... ] } *)
  | p=delimited(L_BRACKET, plus_constr_list, R_BRACKET) { TSum(p)        }

  (* { contrId [ aType ] * constrId [ aType ] [ * ... ] } *)
  | p=delimited(L_BRACKET, star_constr_list, R_BRACKET) { TProd(p)       }

  (* { constrId [ aType ] } *)
  | p=delimited(L_BRACKET, constr, R_BRACKET)           { TSum([p])      }

  (* rec aTypeId is aType *)
  | REC ti=type_id IS t=typ                             { TRec(ti, t)    }

  (* (aType) *)
  | t=delimited(L_PAREN, typ, R_PAREN)                  { t              }

types:
  (* aType, [ aType, aType, ... ] *)
    l=separated_nonempty_list(COMMA, typ) { l }

