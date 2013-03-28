{
 (** This module implements lexical analysis. *)
 open Parser
 }

let var_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' _]*       (** variables *)
let constr_id = ['A'-'Z' _]['A'-'Z' 'a'-'z' '0'-'9' _]*  (** constructors *)
let type_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' _]*      (** types *)
let int_t = ['0'-'9']+ | 0x['0'-'9' 'a'-'f' 'A'-'F']+ | 0b['0' '1']+  (** integer litterals *)
let atom = \\(**?:(?:0|1)?\d{1,2}|2(?:[0-4]\d|5[0-5])) *) 
            | \0x['0'-'9' 'a'-'f' 'A'-'F']\0x['0'-'9' 'a'-'f' 'A'-'F'] 
            | [printable] 
            |'\\' | '\'' | '\n' | '\t' | '\b' | '\r'
let char_t = [atom]                                      (** char litterals *)
let str = [atom]*                                        (** string litterals *)
let layout = [ ' ' '\t' '\r' '\n']

rule main = parse
  layout        { main lexbuf }
| '('           { L_PAREN }
| ')'           { R_PAREN }
| '{'           { L_BRACKET }
| '}'           { R_BRACKET }
| '['           { L_SQUARE }
| ']'           { R_SQUARE }
| '+'           { PLUS }
| '*'           { STAR }
| '-'           { MINUS }
| '/'           { SLASH }
| '%'           { PERCENT }
| '='           { EQ }
| ":="          { ASSIGN }
| "&&"          { DBL_AND }
| "||"          { DBL_PIPE }
| "<="          { LT_EQ }
| ">="          { GT_EQ }
| "!="          { NEG_EQ }
| '<'           { LT }
| '>'           { GT }
| '~'           { TILDE }
| ':'           { COLON }   
| ';'           { SEMICOLON }
| '.'           { DOT }
| ','           { COMMA }
| '_'           { UNDERSC }
| '0'           { ZERO }
| "=>"          { DBL_R_ARROW }
| "->"          { R_ARROW }
| "<-"          { L_ARROW }
| "if"          { IF }
| "then"        { THEN }
| "else"        { ELSE }
| "fun"         { FUN }
| "do"          { DO }
| "case"        { CASE }
| "def"         { DEF }
| "with"        { WITH }
| "at"          { AT }
| "in"          { IN }
| "where"       { WHERE }
| "end"         { END }
| "val"         { VAL }
| "is"          { IS }
| "type"        { TYPE }
| "rec"         { REC }
| "or"          { OR }
| "and"         { AND }
| "not"         { NOT }
| int_t as x    { INT x }
| char_t as x   { CHAR x }
| str as x      { STR x }
| var_id as x   { VAR_ID x }
| type_id as x  { TYPE_ID x }
| const_id as x { CONST_ID x }

