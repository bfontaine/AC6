%{
  (** This module implements syntactic analysis. *)

  open AST
  open Sugar

  let parse_error = Error.error "during parsing"

%}


%token EOF
%token <int> INT
%token <char> CHAR
%token <string> STR
%token <string> VAR_ID
%token <string> TYPE_ID
%token <string> CONSTR_ID
%token PIPE L_PAREN R_PAREN L_BRACKET R_BRACKET L_SQUARE R_SQUARE
%token PLUS STAR EQ (* MINUS SLASH PERCENT ASSIGN
%token DBL_AND DBL_PIPE LT_EQ GT_EQ NEG_EQ LT GT
%token TILDE *) COLON SEMICOLON DOT COMMA UNDERSC ZERO
%token DBL_R_ARROW R_ARROW L_ARROW
%token IF ELSE THEN FUN DO CASE DEF WITH AT IN WHERE END
%token VAL IS TYPE REC OR AND NOT

%start<AST.program> input

%nonassoc CONSTR_ID VAR_ID STR CHAR INT

%nonassoc L_PAREN

%nonassoc DEF FUN CASE AT

%right IN

%left NOT
%right OR
%right AND

%nonassoc IS

%right DBL_R_ARROW
%right R_ARROW

%right SEMICOLON

%left EQ
%left VAL

%nonassoc DO
%nonassoc IF
%nonassoc THEN
%nonassoc ELSE
%nonassoc WHERE

%left DOT

(* highest priority *)
%right EXPR_EXPR

%%

(**
 * a program is a list of definitions
 *)
input:
    p=list(definition) EOF { p }


(** == Bindings == *)

binding:  a=argument_identifier t=option(preceded(COLON, typ)) { Binding(a, t) }
bindings: l=list(delimited(L_PAREN, binding, R_PAREN)) { l }


(** == Branches = *)

branch_list: option(PIPE) bl=branch_list_with_no_pipe { bl }
branch_list_with_no_pipe: l=separated_nonempty_list(PIPE, branch) { l }

branch: p=pattern DBL_R_ARROW e=expr { Branch(p, e) }


(** == Constructors == *)

(* constr_id [type] *)
constr: c=constr_id t=option(typ) { TConstructor(c, t) }

(* contr_id <- expr *)
constr_def: ce=separated_pair(constr_id, L_ARROW , option(expr)) { ce }
constr_defs: l=separated_nonempty_list(SEMICOLON, constr_def) { l }

plus_constr_list: l=separated_list(PLUS, constr) { l }
star_constr_list: l=separated_list(STAR, constr) { l }


(** == Definitions == *)

definition:
(** type definitions *)
      TYPE t1=type_id tl=delimited(L_PAREN, type_ids, R_PAREN) EQ t2=typ { DType(t1, tl, t2) }
(** variable definitions *)
    | v=vdefinition { DVal(v) }

vdefinition:
      VAL b=binding EQ e=expr { Simple(b, e) }
(** function definitions *)
    | DEF v=var_id bl=bindings COLON t=typ EQ e=expr w=with_list
      { MutuallyRecursive((Binding(Named(v), None), mk_fundef bl (Some t) e)::w) }

with_list: l=nonempty_list(with_st) { l }

with_st:
      WITH v=var_id bl=bindings COLON t=typ EQ e=expr { (Binding(Named(v), None), mk_fundef bl (Some t) e) }


(** == Expressions == *)

expr:
    i=INT                                                                                 { EInt(i)                  }
  | c=CHAR                                                                                { EChar(c)                 }
  | s=STR                                                                                 { EString(s)               }
  | v=var_id                                                                              { EVar(v)                  }
  | c=constr_id t=option(preceded(AT, typ)) e=option(delimited(L_SQUARE, expr, R_SQUARE)) { ESum(c, t, e)            }
  | t=option(preceded(AT, typ)) cl=delimited(L_BRACKET, constr_defs, R_BRACKET)           { EProd(t, cl)             }
  | e=delimited(L_PAREN, expr, R_PAREN)                                                   { e                        }
  | L_PAREN e=expr COLON t=typ R_PAREN                                                    { EAnnot(e, t)             }
  | e1=expr SEMICOLON e2=expr                                                             { ESeq([e1; e2])           }
  | v=vdefinition IN e=expr                                                               { EDef(v, e)               }
  | e=expr WHERE v=vdefinition END                                                        { EDef(v, e)               }
  | e1=expr e2=expr %prec EXPR_EXPR                                                       { EApp(e1, e2)             }
  | e1=expr DOT e2=expr                                                                   { EApp(e2, e1)             }
(*| e1=expr o=op e2=expr                                                                  { TODO                     }
  | u=unop e=expr                                                                         { TODO                     } *)
  | CASE t=option(preceded(AT, typ)) b=delimited(L_BRACKET, branch_list, R_BRACKET)       { ECase(t, b)              }
  | IF cond=expr THEN e1=expr ELSE e2=expr                                                { mk_ifthenelse cond e1 e2 }
  | IF cond=expr THEN e1=expr                                                             { mk_ifthen cond e1        }
  | FUN bl=bindings t=option(preceded(COLON, typ)) DBL_R_ARROW e=expr                     { mk_fun bl t e            }
  | DO e=delimited(L_BRACKET, expr, R_BRACKET)                                            { mk_do e                  }


(** == Identifiers == *)

argument_identifier:
      v=var_id { Named(v) }
    | UNDERSC  { Unnamed  }

constr_id: c=CONSTR_ID { CIdentifier(c) }
type_id: t=TYPE_ID     { TIdentifier(t) }
var_id: v=VAR_ID       { Identifier(v)  }

type_ids: l=separated_list(COMMA, type_id) { l }

(** == Operations == *)

(** === Binary Operations === *)

(* op:
    PLUS        { TODO }
  | STAR        { TODO }
  | MINUS       { TODO }
  | SLASH       { TODO }
  | PERCENT     { TODO }
  | EQ          { TODO }
  | ASSIGN      { TODO }
  | DBL_AND     { TODO }
  | DBL_PIPE    { TODO }
  | LT_EQ       { TODO }
  | GT_EQ       { TODO }
  | NEG_EQ      { TODO }
  | GT          { TODO }
  | LT          { TODO }*)

(** === Unary Operations === *)

(* unop:
    MINUS { TODO }
  | TILDE { TODO } *)


(** == Patterns == *)

(* constr_id -> pattern *)
constr_pattern: cp=separated_pair(constr_id, R_ARROW, pattern) { cp }

(* two or more contr_id -> pattern ; ... *)
constr_patterns:
    cp1=constr_pattern SEMICOLON cp2=constr_pattern { [cp1; cp2] }
  | cp=constr_pattern SEMICOLON cps=constr_patterns { cp::cps    }

pattern:
  | c=constr_id t=option(preceded(AT, typ)) p=option(delimited(L_SQUARE, pattern, R_SQUARE)) { PSum(c, t, p) }
  | t=option(preceded(AT, typ)) cp=delimited(L_BRACKET, constr_patterns, R_BRACKET)          { PProd(t, cp)  }
  | p1=pattern OR p2=pattern                                                                 { POr(p1, p2)   }
  | p1=pattern AND p2=pattern                                                                { PAnd(p1, p2)  }
  | NOT p=pattern                                                                            { PNot(p)       }
  | ZERO                                                                                     { PZero         }
  | v=var_id                                                                                 { PVar(v)       }
  | UNDERSC                                                                                  { POne          }


(** == Types == *)

typ:
    ti=type_id tl=delimited(L_PAREN, types, R_PAREN)    { TVar(ti, tl)   }
  | t1=typ R_ARROW t2=typ                               { TArrow(t1, t2) }
  | p=delimited(L_BRACKET, plus_constr_list, R_BRACKET) { TSum(p)        }
  | p=delimited(L_BRACKET, star_constr_list, R_BRACKET) { TProd(p)       }
  | REC ti=type_id IS t=typ                             { TRec(ti, t)    }
  | t=delimited(L_PAREN, typ, R_PAREN)                  { t              }

types: l=separated_list(COMMA, typ) { l }

