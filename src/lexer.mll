{
 (** This module implements lexical analysis. *)
 open Parser
 }

let digit  = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let hex    = ['0'-'9' 'a'-'f' 'A'-'F']

let id = ['a'-'z'] letter*         (** variables & types *)
let constr_id = ['A'-'Z' '_'] letter*  (** constructors *)
let integer = digit+ | "0x" hex+ | "0b" ['0' '1']+  (** integer litterals *)
let atom =
    "\\" ['0' '1'] digit digit | '2' ['0'-'4'] digit | "25" ['0'-'5']
  | "\\0x" hex hex
  | ['\x20'-'\x7E'] (* printable chars *)
  | '\\' | '\'' | '\n' | '\t' | '\b' | '\r'
let character = '\'' atom '\''   (** char litterals *)
let str = '"' atom* '"'        (** string litterals *)
let layout = [ ' ' '\t' '\r' '\n']

rule main = parse
  layout         { main lexbuf }
| '|'            { PIPE }
| '('            { L_PAREN }
| ')'            { R_PAREN }
| '{'            { L_BRACKET }
| '}'            { R_BRACKET }
| '['            { L_SQUARE }
| ']'            { R_SQUARE }
| '+'            { PLUS }
| '*'            { STAR }
| '='            { EQ }
| '-'            { MINUS }
| '/'            { SLASH }
| '%'            { PERCENT }
| ":="           { COLON_EQ }
| "&&"           { ANDAND }
| "||"           { PIPEPIPE }
| "<="           { LE }
| ">="           { GE }
| "!="           { NE }
| '<'            { LT }
| '>'            { GT }
| '~'            { TILDE }
| ':'            { COLON }   
| ';'            { SEMICOLON }
| '.'            { DOT }
| ','            { COMMA }
| '_'            { UNDERSC }
| '0'            { ZERO }
| "=>"           { DBL_R_ARROW }
| "->"           { R_ARROW }
| "<-"           { L_ARROW }
| "if"           { IF }
| "then"         { THEN }
| "else"         { ELSE }
| "fun"          { FUN }
| "do"           { DO }
| "case"         { CASE }
| "def"          { DEF }
| "with"         { WITH }
| "at"           { AT }
| "in"           { IN }
| "where"        { WHERE }
| "end"          { END }
| "val"          { VAL }
| "is"           { IS }
| "type"         { TYPE }
| "rec"          { REC }
| "or"           { OR }
| "and"          { AND }
| "not"          { NOT }
| integer as x   { INT(int_of_string x) }
| character as x { CHAR x.[0] }
| str as x       { STR x }
| id as x        { ID x }
| constr_id as x { CONSTR_ID x }
| "**"           { line_comment lexbuf }
| "(*"           { block_comment 1 lexbuf }
| _              { failwith "Parse error!" }

(**
 * This is a line comment. When the parser sees a "**" token, it enters
 * in this loop, and ignore everything until the next '\r' or '\n' token. After
 * that, it comes back in the main loop.
 **)
and line_comment = parse
  ['\r' '\n']    { main lexbuf }
| _              { line_comment lexbuf }

(**
 * This is a block comment, possibly nested. When the parser sees a "(*", it
 * enters in this loop, and ignore everything except "(*" and "*)". If it sees
 * another "(*" token, it increases the current depth and recursively call this
 * loop. If it sees a "*)" token, it decreases the current depth. If the depth
 * is equal to 0, it comes back in the main loop. "*)"
 **)
and block_comment depth = parse
  "(*"           { block_comment (depth + 1) lexbuf }
| "*)"           { match (depth - 1) with
                   | 0 -> main lexbuf
                   | _ -> block_comment (depth - 1) lexbuf }
| _              { block_comment depth lexbuf }
