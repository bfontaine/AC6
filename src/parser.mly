%{
  (** This module implements syntactic analysis. *)

  open AST

  let parse_error = Error.error "during parsing"

%}


%token EOF

%start<AST.program> program


%%

program: 
   EOF
    {
      failwith "Students, this is your job."
    }

