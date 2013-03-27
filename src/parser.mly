%{
  (** This module implements syntactic analysis. *)

  open AST

  let parse_error = Error.error "during parsing"

%}

(* The lexin phase will produce the following tokens: *)
%toke EOF
%token <string> INT
%token <string> CHAR
%token <string> STR
%token <string> VAR_ID
%token <string> TYPE_ID
%token <string> CONST_ID
%token L_PAREN R_PAREN L_BRACKET R_BRACKET L_SQUARE R_SQUARE
%token PLUS STAR MINUS SLASH PERCENT EQ ASSIGN
%tocken DBL_AND DBL_PIPE LT_EQ GT_EQ NEG_EQ LT GT
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

