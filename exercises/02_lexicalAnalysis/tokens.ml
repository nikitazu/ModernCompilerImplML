type token =
(* Keywords *)
| WHILE
| FOR
| TO
| BREAK
| LET
| IN
| END
| FUNCTION
| VAR
| TYPE
| ARRAY
| IF
| THEN
| ELSE
| DO
| OF
| NIL
(* Punctuation *)
| COMMA
| COLUMN
| SEMICOL
| LPAREN
| RPAREN
| LSQUAREB
| RSQUAREB
| LCURLYB
| RCURLYB
| DOT
| PLUS
| MINUS
| MUL
| DIV
| EQ
| NOTEQ
| LESS
| LESSEQ
| GREATER
| GREATEREQ
| AND
| OR
| ASSIGN
(* Literals  *)
| NUM of int
| REAL of float
| STRING of string
(* Identifier *)
| ID of string


(* Keywords *)
let t_while     = WHILE
let t_for       = FOR
let t_to        = TO
let t_break     = BREAK
let t_let       = LET
let t_in        = IN
let t_end       = END
let t_function  = FUNCTION
let t_var       = VAR
let t_type      = TYPE
let t_array     = ARRAY
let t_if        = IF
let t_then      = THEN
let t_else      = ELSE
let t_do        = DO
let t_of        = OF
let t_nil       = NIL
(* Punctuation *)
let t_comma     = COMMA
let t_column    = COLUMN
let t_semicol   = SEMICOL
let t_lparen    = LPAREN
let t_rparen    = RPAREN
let t_lsquareb  = LSQUAREB
let t_rsquareb  = RSQUAREB
let t_lcurlyb   = LCURLYB
let t_rcurlyb   = RCURLYB
let t_dot       = DOT
let t_plus      = PLUS
let t_minus     = MINUS
let t_mul       = MUL
let t_div       = DIV
let t_eq        = EQ
let t_noteq     = NOTEQ
let t_less      = LESS
let t_lesseq    = LESSEQ
let t_greater   = GREATER
let t_greatereq = GREATEREQ
let t_and       = AND
let t_or        = OR
let t_assign    = ASSIGN
(* Literals  *)
let t_num n = NUM n
let t_real n = REAL n
let t_string s = STRING s
(* Identifier *)
let t_id id = ID id


let string_of_token = function
  (* Keywords *)
  | WHILE      -> "WHILE"
  | FOR        -> "FOR"
  | TO         -> "TO"
  | BREAK      -> "BREAK"
  | LET        -> "LET"
  | IN         -> "IN"
  | END        -> "END"
  | FUNCTION   -> "FUNCTION"
  | VAR        -> "VAR"
  | TYPE       -> "TYPE"
  | ARRAY      -> "ARRAY"
  | IF         -> "IF"
  | THEN       -> "THEN"
  | ELSE       -> "ELSE"
  | DO         -> "DO"
  | OF         -> "OF"
  | NIL        -> "NIL"
  (* Punctuation *)
  | COMMA      -> "COMMA"
  | COLUMN     -> "COLUMN"
  | SEMICOL    -> "SEMICOL"
  | LPAREN     -> "LPAREN"
  | RPAREN     -> "RPAREN"
  | LSQUAREB   -> "LSQUAREB"
  | RSQUAREB   -> "RSQUAREB"
  | LCURLYB    -> "LCURLYB"
  | RCURLYB    -> "RCURLYB"
  | DOT        -> "DOT"
  | PLUS       -> "PLUS"
  | MINUS      -> "MINUS"
  | MUL        -> "MUL"
  | DIV        -> "DIV"
  | EQ         -> "EQ"
  | NOTEQ      -> "NOTEQ"
  | LESS       -> "LESS"
  | LESSEQ     -> "LESSEQ"
  | GREATER    -> "GREATER"
  | GREATEREQ  -> "GREATEREQ"
  | AND        -> "AND"
  | OR         -> "OR"
  | ASSIGN     -> "ASSIGN"
  (* Literals  *)
  | NUM n      -> "NUM=" ^ string_of_int n
  | REAL n     -> "REAL=" ^ string_of_float n
  | STRING s   -> "STRING=" ^ s
  (* Identifier *)
  | ID id      -> "ID=" ^ id
