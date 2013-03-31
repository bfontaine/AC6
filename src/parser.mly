%{
  (** This module implements syntactic analysis. *)

  open AST

  let parse_error = Error.error "during parsing"

%}


%token EOF
%token <string> INT
%token <string> CHAR
%token <string> STR
%token <string> VAR_ID
%token <string> TYPE_ID
%token <string> CONSTR_ID
%token PIPE L_PAREN R_PAREN L_BRACKET R_BRACKET L_SQUARE R_SQUARE
%token PLUS STAR MINUS SLASH PERCENT EQ ASSIGN
%token DBL_AND DBL_PIPE LT_EQ GT_EQ NEG_EQ LT GT
%token TILDE COLON SEMICOLON DOT COMMA UNDERSC ZERO
%token DBL_R_ARROW R_ARROW L_ARROW
%token IF ELSE THEN FUN DO CASE DEF WITH AT IN WHERE END
%token VAL IS TYPE REC OR AND NOT

%start<AST.program> input


%%

(* a program is a list of definitions *)
input:
    p=list(definition) EOF { p }

var_id: v=VAR_ID       { Identifier(v)  }
type_id: t=TYPE_ID     { TIdentifier(t) }
constr_id: c=CONSTR_ID { CIdentifier(c) }

(* == Lists == *)

(* -- Possibly empty -- *)

definitions: l=list(definition) { l }

plus_constr_list: l=separated_list(PLUS, constr) { l }
star_constr_list: l=separated_list(STAR, constr) { l }

(* -- Non empty -- *)

bindings: l=nonempty_list(paren_binding) { l }
types: l=separated_nonempty_list(COMMA, type_id) { l }
constr_defs: l=separated_nonempty_list(SEMICOLON, constr_def) { l }

branch_list_with_no_pipe: l=separated_nonempty_list(PIPE, branch) { l }

(* ==== *)

definition:
      TYPE t1=type_id                          EQ t2=typ { DType(t1, [], t2) }
    | TYPE t1=type_id L_PAREN tl=types R_PAREN EQ t2=typ { DType(t1, tl, t2) }
    | v=vdefinition                                      { DVal(v)           }

vdefinition:
      VAL b=binding EQ e=expr { Simple(b, e) }
    (* FIXME: this doesn't use v1, bl, t1, e. See sugar.mli l.35 *)
    | DEF v1=var_id bl=bindings COLON t1=typ EQ e=expr w=with_list { MutuallyRecursive(w) }

binding:
      a=argument_identifier COLON t=typ { Binding(a, t)    }
    | a=argument_identifier             { Binding(a, None) }

paren_binding: L_PAREN b=binding R_PAREN { b }

(* list of one or more 'with' statements *)
with_list:
      (* nothing *)          { []    }
    | w=with_st wl=with_list { w::wl }

with_st:
    (* FIXME: this doesn't use bl *)
      WITH v=var_id bl=bindings COLON t=typ EQ e=expr { (Binding(v, t), e) }

argument_identifier:
      v=var_id { Named(v) }
    | UNDERSC  { Unnamed  }

typ:
    ti=type_id                              { TVar(ti, [])   }
  | ti=type_id L_PAREN tl=types R_PAREN     { TVAR(ti, tl)   }
  | t1=typ R_ARROW t2=typ                   { TArrow(t1, t2) }
  | L_BRACKET p=plus_constr_list R_BRACKET  { TSum(p)        }
  | L_BRACKET p=star_constr_list R_BRACKET  { TProd(p)       }
  | REC ti=type_id IS t=typ                 { TRec(ti, t)    }
  | L_PAREN t=typ R_PAREN                   { t              }

(* constr_id [type] *)
constr:
    c=constr_id       { TConstructor(c, None) }
  | c=constr_id t=typ { TConstructor(c, t)    }

(* contr_id <- expr *)
constr_def: c=constr_id L_ARROW e=expr { (c, e) }

expr:
    i=INT                                            { EInt(i)             }
  | c=CHAR                                           { EChar(c)            }
  | s=STR                                            { EString(s)          }
  | v=var_id                                         { EVar(v)             }
  | c=constr_id                                      { ESum(c, None, None) }
  | c=constr_id          L_SQUARE e=expr R_SQUARE    { ESum(c, None, e)    }
  | c=constr_id AT t=typ                             { ESum(c, t, None)    }
  | c=constr_id AT t=typ L_SQUARE e=expr R_SQUARE    { ESum(c, t, e)       }
  |          L_BRACKET cl=constr_defs R_BRACKET      { Eprod(None, cl)     }
  | AT t=typ L_BRACKET cl=constr_defs R_BRACKET      { Eprod(t, cl)        }
  | L_PAREN e=expr R_PAREN                           { e                   }
  | L_PAREN e=expr COLON t=typ R_PAREN               { EAnnot(e, t)        }
  | e1=expr SEMICOLON e2=expr                        { ESeq(e1, e2)        }
  | v=vdefinition IN e=expr                          { EDef(v, e)          }
  | e=expr WHERE v=vdefinition END                   { EDef(v, e)          }
  | e1=expr e2=expr                                  { EApp(e1, e2)        }
  | e1=expr DOT e2=expr                              { EApp(e2, e1)        }
  | e1=expr o=op e2=expr                             { (*TODO*)            }
  | u=unop e=expr                                    { (*TODO*)            }
  | CASE           L_BRACKET b=branch_list R_BRACKET { ECase(None, b)      }
  | CASE AT t=typ  L_BRACKET b=branch_list R_BRACKET { ECase(t, b)         }
  | IF cond=expr THEN e1=expr                        { mk_ifthen cond e1   }
  | IF cond=expr THEN e1=expr ELSE e2=expr           { mk_ifthenelse cond e1 e2 }
  | FUN bl=bindings             DBL_R_ARROW e=expr   { mk_fun bl None e    }
  | FUN bl=bindings COLON t=typ DBL_R_ARROW e=expr   { mk_fun bl t e       }
  | DO L_BRACKET e=expr R_BRACKET                    { mk_do e }

op:
    PLUS        { (*TODO*) }
  | STAR        { (*TODO*) }
  | MINUS       { (*TODO*) }
  | SLASH       { (*TODO*) }
  | PERCENT     { (*TODO*) }
  | EQ          { (*TODO*) }
  | ASSIGN      { (*TODO*) }
  | DBL_AND     { (*TODO*) }
  | DBL_PIPE    { (*TODO*) }
  | LT_EQ       { (*TODO*) }
  | GT_EQ       { (*TODO*) }
  | NEG_EQ      { (*TODO*) }
  | GT          { (*TODO*) }
  | LT          { (*TODO*) }

unop:
    MINUS { (*TODO*) }
  | TILDE { (*TODO*) }

(* one or more branches *)
branch_list: option(PIPE) bl=branch_list_with_no_pipe { bl }

branch: p=pattern DBL_R_ARROW e=expr { (*TODO*) }

(* constr_id -> pattern *)
constr_pattern: c=constr_id R_ARROW p=pattern { (c, p) }

(* two or more contr_id -> pattern ; ... *)
constr_patterns:
    cp1=constr_pattern SEMICOLON cp2=constr_pattern { [cp1, cp2] }
  | cp=constr_pattern SEMICOLON cps=constr_patterns { cp::cps    }

pattern:
    c=constr_id                                      { PSum(c, None, None) }
  | c=constr_id          L_SQUARE p=pattern R_SQUARE { Psum(c, None, p)    }
  | c=constr_id AT t=typ                             { PSum(c, t, None)    }
  | c=constr_id AT t=typ L_SQUARE p=pattern R_SQUARE { PSum(c, t, p)       }
  |          L_BRACKET cp=constr_patterns R_BRACKET  { PProd(None, cp)     }
  | AT t=typ L_BRACKET cp=constr_patterns R_BRACKET  { PProd(t, cp)        }
  | p1=pattern OR p2=pattern                         { POr(p1, p2)         }
  | p1=pattern AND p2=pattern                        { PAnd(p1, p2)        }
  | NOT p=pattern                                    { PNot(p)             }
  | ZERO                                             { PZero               }
  | v=var_id                                         { PVar(v)             }
  | UNDERSC                                          { POne                }

