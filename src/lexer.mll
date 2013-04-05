{
 (** This module implements lexical analysis. *)
 open Parser
 }

let d = ['0'-'9']
let w = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let h = ['0'-'9' 'a'-'f' 'A'-'F']

let var_id = ['a'-'z'] w*         (** variables *)
let constr_id = ['A'-'Z' '_'] w*  (** constructors *)
let type_id = var_id              (** types *)
let integer = d+ | "0x" h+ | "0b" ['0' '1']+  (** integer litterals *)
let atom =
    "\\" ['0' '1'] d d | '2' ['0'-'4'] d | "25" ['0'-'5']
  | "\\0x" h h
  | ['\x20'-'\x7E'] (* printable chars *)
  | '\\' | '\'' | '\n' | '\t' | '\b' | '\r'
let character = '\'' atom '\''   (** char litterals *)
let str = '"' atom* '"'        (** string litterals *)
let layout = [ ' ' '\t' '\r' '\n']

rule main = parse
  layout        { main lexbuf }
| '|'           { PIPE }
| '('           { L_PAREN }
| ')'           { R_PAREN }
| '{'           { L_BRACKET }
| '}'           { R_BRACKET }
| '['           { L_SQUARE }
| ']'           { R_SQUARE }
| '+'           { PLUS }
| '*'           { STAR }
| '='           { EQ }
| '-'           { MINUS }
| '/'           { SLASH }
| '%'           { PERCENT }
| ":="          { COLON_EQ }
| "&&"          { ANDAND }
| "||"          { PIPEPIPE }
| "<="          { LE }
| ">="          { GE }
| "!="          { NE }
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
| integer as x  { INT x }
| character as x { CHAR x }
| str as x      { STR x }
| var_id as x   { VAR_ID x }
| type_id as x  { TYPE_ID x }
| constr_id as x { CONSTR_ID x }

