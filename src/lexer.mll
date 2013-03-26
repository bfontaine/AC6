{
 (** This module implements lexical analysis. *)

 }

let var_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' _]*       (** Identificateur de variables *)
let constr_id = ['A'-'Z' _]['A'-'Z' 'a'-'z' '0'-'9' _]*  (** Identificateur de constructeurs de données *)
let type_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' _]*      (** Identificateur de type *)
let int_t = ['0'-'9']+ | 0x['0'-'9' 'a'-'f' 'A'-'F']+ | 0b['0' '1']+  (** Littéraux entiers *)
let atom = \\(**?:(?:0|1)?\d{1,2}|2(?:[0-4]\d|5[0-5])) *) 
            | \0x['0'-'9' 'a'-'f' 'A'-'F'] 
            | [printable] 
            |'\\' | '\'' | '\n' | '\t' | '\b' | '\r'
let char_t = [atom]                                      (** Littéraux caractères*)
let str = [atom]*                                        (** Littéraux chaîne de caractère *)
let layout = [ ' ' '\t' '\r' '\n']

rule main = parse
  layout        { main lexbuf }
| '('           {}
| ')'           {}
| '{'           {}
| '}'           {}
| '['           {}
| ']'           {}
| '+'           {}
| '*'           {}
| '-'           {}
| '/'           {}
| '%'           {}
| '='           {}
| ":="          {}
| "&&"          {}
| "||"          {}
| "<="          {}
| ">="          {}
| '<'           {}
| '>'           {}
| "!="          {}
| '~'           {}
| ':'           {}
| "=>"          {}
| "if"          {}
| "then"        {}
| "else"        {}
| "fun"         {}
| "do"          {}
| "case"        {}
| int_t as x    { INT x }
| char_t as x   { CHAR x }
| str as x      { STR x }
| var_id as x   { VAR_ID x }

| _                                     
{ failwith "Students, this is your job."
}


