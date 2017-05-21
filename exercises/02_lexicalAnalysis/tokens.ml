type line = int
type pos  = int
type meta = line * pos

type tokenvalue =
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
| COLON
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

type token = tokenvalue * meta


let meta_of_lexbuf lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  (p.pos_lnum, p.pos_cnum - p.pos_bol)

let token_of_meta t lexbuf =
  (t, meta_of_lexbuf lexbuf)

(* Keywords *)
let t_while     = token_of_meta WHILE
let t_for       = token_of_meta FOR      
let t_to        = token_of_meta TO       
let t_break     = token_of_meta BREAK    
let t_let       = token_of_meta LET      
let t_in        = token_of_meta IN       
let t_end       = token_of_meta END      
let t_function  = token_of_meta FUNCTION 
let t_var       = token_of_meta VAR      
let t_type      = token_of_meta TYPE     
let t_array     = token_of_meta ARRAY    
let t_if        = token_of_meta IF       
let t_then      = token_of_meta THEN     
let t_else      = token_of_meta ELSE     
let t_do        = token_of_meta DO       
let t_of        = token_of_meta OF       
let t_nil       = token_of_meta NIL      
(* Punctuation *)
let t_comma     = token_of_meta COMMA     
let t_colon     = token_of_meta COLON     
let t_semicol   = token_of_meta SEMICOL   
let t_lparen    = token_of_meta LPAREN    
let t_rparen    = token_of_meta RPAREN    
let t_lsquareb  = token_of_meta LSQUAREB  
let t_rsquareb  = token_of_meta RSQUAREB  
let t_lcurlyb   = token_of_meta LCURLYB   
let t_rcurlyb   = token_of_meta RCURLYB   
let t_dot       = token_of_meta DOT       
let t_plus      = token_of_meta PLUS      
let t_minus     = token_of_meta MINUS     
let t_mul       = token_of_meta MUL       
let t_div       = token_of_meta DIV       
let t_eq        = token_of_meta EQ        
let t_noteq     = token_of_meta NOTEQ     
let t_less      = token_of_meta LESS      
let t_lesseq    = token_of_meta LESSEQ    
let t_greater   = token_of_meta GREATER   
let t_greatereq = token_of_meta GREATEREQ 
let t_and       = token_of_meta AND       
let t_or        = token_of_meta OR        
let t_assign    = token_of_meta ASSIGN    
(* Literals  *)
let t_num     n = token_of_meta (NUM n)
let t_real    n = token_of_meta (REAL n)
let t_string  s = token_of_meta (STRING s)
(* Identifier *)
let t_id     id = token_of_meta (ID id)


let string_of_meta (line, pos) =
  "(" ^ string_of_int line ^ "," ^ string_of_int pos ^ ")"

let string_of_tokenvalue = function
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
  | COLON      -> "COLON"    
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
  | NUM     n -> "NUM=" ^ string_of_int n
  | REAL    n -> "REAL=" ^ string_of_float n
  | STRING  s -> "STRING=" ^ s
  (* Identifier *)
  | ID     id -> "ID=" ^ id


let string_of_token (t, m) =
  string_of_tokenvalue t ^ " " ^ string_of_meta m

