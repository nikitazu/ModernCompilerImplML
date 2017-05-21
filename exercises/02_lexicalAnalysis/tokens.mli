type line
type pos
type meta
type token

(* Keywords *)
val t_while     : Lexing.lexbuf -> token
val t_for       : Lexing.lexbuf -> token
val t_to        : Lexing.lexbuf -> token
val t_break     : Lexing.lexbuf -> token
val t_let       : Lexing.lexbuf -> token
val t_in        : Lexing.lexbuf -> token
val t_end       : Lexing.lexbuf -> token
val t_function  : Lexing.lexbuf -> token
val t_var       : Lexing.lexbuf -> token
val t_type      : Lexing.lexbuf -> token
val t_array     : Lexing.lexbuf -> token
val t_if        : Lexing.lexbuf -> token
val t_then      : Lexing.lexbuf -> token
val t_else      : Lexing.lexbuf -> token
val t_do        : Lexing.lexbuf -> token
val t_of        : Lexing.lexbuf -> token
val t_nil       : Lexing.lexbuf -> token
(* Punctuation *)
val t_comma     : Lexing.lexbuf -> token
val t_colon     : Lexing.lexbuf -> token
val t_semicol   : Lexing.lexbuf -> token
val t_lparen    : Lexing.lexbuf -> token
val t_rparen    : Lexing.lexbuf -> token
val t_lsquareb  : Lexing.lexbuf -> token
val t_rsquareb  : Lexing.lexbuf -> token
val t_lcurlyb   : Lexing.lexbuf -> token
val t_rcurlyb   : Lexing.lexbuf -> token
val t_dot       : Lexing.lexbuf -> token
val t_plus      : Lexing.lexbuf -> token
val t_minus     : Lexing.lexbuf -> token
val t_mul       : Lexing.lexbuf -> token
val t_div       : Lexing.lexbuf -> token
val t_eq        : Lexing.lexbuf -> token
val t_noteq     : Lexing.lexbuf -> token
val t_less      : Lexing.lexbuf -> token
val t_lesseq    : Lexing.lexbuf -> token
val t_greater   : Lexing.lexbuf -> token
val t_greatereq : Lexing.lexbuf -> token
val t_and       : Lexing.lexbuf -> token
val t_or        : Lexing.lexbuf -> token
val t_assign    : Lexing.lexbuf -> token
(* Literals  *)
val t_num       : int -> Lexing.lexbuf    -> token
val t_real      : float -> Lexing.lexbuf  -> token
val t_string    : string -> Lexing.lexbuf -> token
(* Identifier *)
val t_id        : string -> Lexing.lexbuf -> token

val string_of_token : token -> string
