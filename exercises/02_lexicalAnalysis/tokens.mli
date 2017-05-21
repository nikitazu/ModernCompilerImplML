type token

(* Keywords *)
val t_while     : token
val t_for       : token
val t_to        : token
val t_break     : token
val t_let       : token
val t_in        : token
val t_end       : token
val t_function  : token
val t_var       : token
val t_type      : token
val t_array     : token
val t_if        : token
val t_then      : token
val t_else      : token
val t_do        : token
val t_of        : token
val t_nil       : token
(* Punctuation *)
val t_comma     : token
val t_colon     : token
val t_semicol   : token
val t_lparen    : token
val t_rparen    : token
val t_lsquareb  : token
val t_rsquareb  : token
val t_lcurlyb   : token
val t_rcurlyb   : token
val t_dot       : token
val t_plus      : token
val t_minus     : token
val t_mul       : token
val t_div       : token
val t_eq        : token
val t_noteq     : token
val t_less      : token
val t_lesseq    : token
val t_greater   : token
val t_greatereq : token
val t_and       : token
val t_or        : token
val t_assign    : token
(* Literals  *)
val t_num       : int    -> token
val t_real      : float  -> token
val t_string    : string -> token
(* Identifier *)
val t_id        : string -> token

val string_of_token : token -> string
