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
%token 
(* The lexing phase will produce the following tokens: *)
%start<AST.program> program


%%

program: 
   EOF
    {
      failwith "Students, this is your job."
    }

