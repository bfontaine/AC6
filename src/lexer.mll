{
 (** This module implements lexical analysis. *)

 }

let var_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' _]*       (** Identificateur de variables *)
let constr_id = ['A'-'Z' _]['A'-'Z' 'a'-'z' '0'-'9' _]*  (** Identificateur de constructeurs de données *)

rule main = parse

| _                                     
{ failwith "Students, this is your job."
}


