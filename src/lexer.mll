{
 (** This module implements lexical analysis. *)

 }

let var_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' _]*       (** Identificateur de variables *)
let constr_id = ['A'-'Z' _]['A'-'Z' 'a'-'z' '0'-'9' _]*  (** Identificateur de constructeurs de données *)
let type_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' _]*      (** Identificateur de type *)
let inte = ['0'-'9']+ | 0x['0'-'9' 'a'-'f' 'A'-'F']+ | 0b['0' '1']+  (** Littéraux entiers *)
(**
let atom = \000 | ... |\255 |\0x[0-9 a-f A-F]2| [printable]|\\|\’|\n|\t|\b|\r
let charc = [atom] (** Littéraux caractères*)
let string = [atom]* (** Littéraux chaîne de caractère *)
*)

rule main = parse

| _                                     
{ failwith "Students, this is your job."
}


