%{
  (** This module implements syntactic analysis. *)

  open AST

  let parse_error = Error.error "during parsing"

%}

(* The lexin phase will produce the following tokens: *)
%token EOF
%token <string> INT
%token <string> CHAR
%token <string> STR
%token <string> VAR_ID
%token <string> TYPE_ID
%token <string> CONST_ID
%token L_PAREN R_PAREN L_BRACKET R_BRACKET L_SQUARE R_SQUARE
%token PLUS STAR MINUS SLASH PERCENT EQ ASSIGN
%token DBL_AND DBL_PIPE LT_EQ GT_EQ NEG_EQ LT GT
%token TILDE COLON SEMICOLON DOT COMMA UNDERSC VIDE
%token DBL_R_ARROW R_ARROW L_ARROW
%token IF ELSE THEN FUN DO CASE DEF WITH AT IN WHERE END
%token VAL IS TYPE REC OR AND NOT

(* The lexing phase will produce the following tokens: *)
%start<AST.program> input


%%
(* The lexing phase will produce the following tokens: *)
input: p=program EOF { p }


program: 
   EOF

    {
      failwith "Students, this is your job."
    }

definition:
    | TYPE type_id=TYPE_ID EQ ty=typ                                     {DType(type_id,[],ty)}
    | TYPE type_id=TYPE_ID L_PAREN (*TODO Type list *) R_PAREN EQ ty=typ {DType(type_id,[],ty)}
    | v=vdefinition                                                      {DVal(v)}

vdefinition:
    | VAL bind=binding EQ e=exp {Simple(bind,e)}
    | DEF var_t=VAR_ID (* TODO binding list *) COLON ty=typ EQ e=exp (*TODO with var_id binding list *) {}

binding:
    | arg=argument_identifier COLON ty=typ {Binding(arg,ty)}
    | arg=argument_identifier              {Binding(arg,None)}

argument_identifier:
    | var_id=VAR_ID {Named(var_id)}
    | UNDERSC       {Unnamed}

type_identifiers:
    | type_id=type_identifier 
    |

type_identifier:
    | type_id=TYPE_ID {TIdentifier(type_id)} 
    
