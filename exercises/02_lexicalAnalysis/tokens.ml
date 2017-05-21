type line = int
type pos  = int
type meta = line * pos

type token =
(* Keywords *)
| WHILE of meta
| FOR of meta
| TO of meta
| BREAK of meta
| LET of meta
| IN of meta
| END of meta
| FUNCTION of meta
| VAR of meta
| TYPE of meta
| ARRAY of meta
| IF of meta
| THEN of meta
| ELSE of meta
| DO of meta
| OF of meta
| NIL of meta
(* Punctuation *)
| COMMA of meta
| COLON of meta
| SEMICOL of meta
| LPAREN of meta
| RPAREN of meta
| LSQUAREB of meta
| RSQUAREB of meta
| LCURLYB of meta
| RCURLYB of meta
| DOT of meta
| PLUS of meta
| MINUS of meta
| MUL of meta
| DIV of meta
| EQ of meta
| NOTEQ of meta
| LESS of meta
| LESSEQ of meta
| GREATER of meta
| GREATEREQ of meta
| AND of meta
| OR of meta
| ASSIGN of meta
(* Literals  *)
| NUM of int * meta
| REAL of float * meta 
| STRING of string * meta
(* Identifier *)
| ID of string * meta


let meta_of_lexbuf lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  (p.pos_lnum, p.pos_cnum - p.pos_bol)

(* Keywords *)
let t_while     lexbuf = WHILE      (meta_of_lexbuf lexbuf)
let t_for       lexbuf = FOR        (meta_of_lexbuf lexbuf)
let t_to        lexbuf = TO         (meta_of_lexbuf lexbuf)
let t_break     lexbuf = BREAK      (meta_of_lexbuf lexbuf)
let t_let       lexbuf = LET        (meta_of_lexbuf lexbuf)
let t_in        lexbuf = IN         (meta_of_lexbuf lexbuf)
let t_end       lexbuf = END        (meta_of_lexbuf lexbuf)
let t_function  lexbuf = FUNCTION   (meta_of_lexbuf lexbuf)
let t_var       lexbuf = VAR        (meta_of_lexbuf lexbuf)
let t_type      lexbuf = TYPE       (meta_of_lexbuf lexbuf)
let t_array     lexbuf = ARRAY      (meta_of_lexbuf lexbuf)
let t_if        lexbuf = IF         (meta_of_lexbuf lexbuf)
let t_then      lexbuf = THEN       (meta_of_lexbuf lexbuf)
let t_else      lexbuf = ELSE       (meta_of_lexbuf lexbuf)
let t_do        lexbuf = DO         (meta_of_lexbuf lexbuf)
let t_of        lexbuf = OF         (meta_of_lexbuf lexbuf)
let t_nil       lexbuf = NIL        (meta_of_lexbuf lexbuf)
(* Punctuation *)
let t_comma     lexbuf = COMMA       (meta_of_lexbuf lexbuf)
let t_colon     lexbuf = COLON       (meta_of_lexbuf lexbuf)
let t_semicol   lexbuf = SEMICOL     (meta_of_lexbuf lexbuf)
let t_lparen    lexbuf = LPAREN      (meta_of_lexbuf lexbuf)
let t_rparen    lexbuf = RPAREN      (meta_of_lexbuf lexbuf)
let t_lsquareb  lexbuf = LSQUAREB    (meta_of_lexbuf lexbuf)
let t_rsquareb  lexbuf = RSQUAREB    (meta_of_lexbuf lexbuf)
let t_lcurlyb   lexbuf = LCURLYB     (meta_of_lexbuf lexbuf)
let t_rcurlyb   lexbuf = RCURLYB     (meta_of_lexbuf lexbuf)
let t_dot       lexbuf = DOT         (meta_of_lexbuf lexbuf)
let t_plus      lexbuf = PLUS        (meta_of_lexbuf lexbuf)
let t_minus     lexbuf = MINUS       (meta_of_lexbuf lexbuf)
let t_mul       lexbuf = MUL         (meta_of_lexbuf lexbuf)
let t_div       lexbuf = DIV         (meta_of_lexbuf lexbuf)
let t_eq        lexbuf = EQ          (meta_of_lexbuf lexbuf)
let t_noteq     lexbuf = NOTEQ       (meta_of_lexbuf lexbuf)
let t_less      lexbuf = LESS        (meta_of_lexbuf lexbuf)
let t_lesseq    lexbuf = LESSEQ      (meta_of_lexbuf lexbuf)
let t_greater   lexbuf = GREATER     (meta_of_lexbuf lexbuf)
let t_greatereq lexbuf = GREATEREQ   (meta_of_lexbuf lexbuf)
let t_and       lexbuf = AND         (meta_of_lexbuf lexbuf)
let t_or        lexbuf = OR          (meta_of_lexbuf lexbuf)
let t_assign    lexbuf = ASSIGN      (meta_of_lexbuf lexbuf)
(* Literals  *)
let t_num     n lexbuf = NUM     (n, (meta_of_lexbuf lexbuf))
let t_real    n lexbuf = REAL    (n, (meta_of_lexbuf lexbuf))
let t_string  s lexbuf = STRING  (s, (meta_of_lexbuf lexbuf))
(* Identifier *)
let t_id     id lexbuf = ID     (id, (meta_of_lexbuf lexbuf))


let string_of_meta (line, pos) =
  "(" ^ (string_of_int line) ^ "," ^ (string_of_int pos) ^ ")"

let string_of_token = function
  (* Keywords *)
  | WHILE      m -> "WHILE"      ^ " " ^ (string_of_meta m)
  | FOR        m -> "FOR"        ^ " " ^ (string_of_meta m)
  | TO         m -> "TO"         ^ " " ^ (string_of_meta m)
  | BREAK      m -> "BREAK"      ^ " " ^ (string_of_meta m)
  | LET        m -> "LET"        ^ " " ^ (string_of_meta m)
  | IN         m -> "IN"         ^ " " ^ (string_of_meta m)
  | END        m -> "END"        ^ " " ^ (string_of_meta m)
  | FUNCTION   m -> "FUNCTION"   ^ " " ^ (string_of_meta m)
  | VAR        m -> "VAR"        ^ " " ^ (string_of_meta m)
  | TYPE       m -> "TYPE"       ^ " " ^ (string_of_meta m)
  | ARRAY      m -> "ARRAY"      ^ " " ^ (string_of_meta m)
  | IF         m -> "IF"         ^ " " ^ (string_of_meta m)
  | THEN       m -> "THEN"       ^ " " ^ (string_of_meta m)
  | ELSE       m -> "ELSE"       ^ " " ^ (string_of_meta m)
  | DO         m -> "DO"         ^ " " ^ (string_of_meta m)
  | OF         m -> "OF"         ^ " " ^ (string_of_meta m)
  | NIL        m -> "NIL"        ^ " " ^ (string_of_meta m)
  (* Punctuation *)
  | COMMA      m -> "COMMA"      ^ " " ^ (string_of_meta m)
  | COLON      m -> "COLON"      ^ " " ^ (string_of_meta m)
  | SEMICOL    m -> "SEMICOL"    ^ " " ^ (string_of_meta m)
  | LPAREN     m -> "LPAREN"     ^ " " ^ (string_of_meta m)
  | RPAREN     m -> "RPAREN"     ^ " " ^ (string_of_meta m)
  | LSQUAREB   m -> "LSQUAREB"   ^ " " ^ (string_of_meta m)
  | RSQUAREB   m -> "RSQUAREB"   ^ " " ^ (string_of_meta m)
  | LCURLYB    m -> "LCURLYB"    ^ " " ^ (string_of_meta m)
  | RCURLYB    m -> "RCURLYB"    ^ " " ^ (string_of_meta m)
  | DOT        m -> "DOT"        ^ " " ^ (string_of_meta m)
  | PLUS       m -> "PLUS"       ^ " " ^ (string_of_meta m)
  | MINUS      m -> "MINUS"      ^ " " ^ (string_of_meta m)
  | MUL        m -> "MUL"        ^ " " ^ (string_of_meta m)
  | DIV        m -> "DIV"        ^ " " ^ (string_of_meta m)
  | EQ         m -> "EQ"         ^ " " ^ (string_of_meta m)
  | NOTEQ      m -> "NOTEQ"      ^ " " ^ (string_of_meta m)
  | LESS       m -> "LESS"       ^ " " ^ (string_of_meta m)
  | LESSEQ     m -> "LESSEQ"     ^ " " ^ (string_of_meta m)
  | GREATER    m -> "GREATER"    ^ " " ^ (string_of_meta m)
  | GREATEREQ  m -> "GREATEREQ"  ^ " " ^ (string_of_meta m)
  | AND        m -> "AND"        ^ " " ^ (string_of_meta m)
  | OR         m -> "OR"         ^ " " ^ (string_of_meta m)
  | ASSIGN     m -> "ASSIGN"     ^ " " ^ (string_of_meta m)
  (* Literals  *)
  | NUM     (n,m) -> "NUM=" ^ string_of_int n     ^ " " ^ (string_of_meta m)
  | REAL    (n,m) -> "REAL=" ^ string_of_float n  ^ " " ^ (string_of_meta m)
  | STRING  (s,m) -> "STRING=" ^ s                ^ " " ^ (string_of_meta m)
  (* Identifier *)
  | ID     (id,m) -> "ID=" ^ id                   ^ " " ^ (string_of_meta m)

