(** This module defines the syntactic sugars. *)

open AST

(** [if e1 then e2 else e3] is desugared using a 
    pattern matching. *)
val mk_ifthenelse : expr -> expr -> expr -> expr

(** [if e1 then e2] is a synonymous for 
    [if e1 then e2 else (U at { U })]. *)
val mk_ifthen : expr -> expr -> expr

(** [fun b1 b2 b3 : ty => e] is desugared into
    [fun b1 => fun b2 => fun b3 => (e : ty)] *)
val mk_fun : binding list -> typ option -> expr -> expr

(** [e1 . e2] is desugared into [e2 e1]. *)
val mk_postfix_application : expr -> expr -> expr

(** [e where vdef] is desugared into [vdef in e] *)
val mk_where : expr -> vdefinition -> expr

(** [do { e }] is desugared into [fun (_ : { U }) => e] *)
val mk_do : expr -> expr

(** [def x1 : ty1 = e1 with x2 : ty2 = e2 with ...] must
    be desugared into
    [def x1 (_ : { U }) = (e1 : ty1) 
     with x2 (_ : { U }) = (e2 : ty2) with ...
    ]. 
    
    So, [mk_fundef bs ty e] introduces an extra binding
    of the form [(_ : { U })] if [bs] is empty. 
*)
val mk_fundef : binding list -> typ option -> expr -> expr

