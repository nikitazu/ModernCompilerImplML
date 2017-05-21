{
type pos = int
exception Eof
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
(* Keywords *)
| "while"         { Tokens.t_while }
| "for"           { Tokens.t_for }
| "to"            { Tokens.t_to }
| "break"         { Tokens.t_break }
| "let"           { Tokens.t_let }
| "in"            { Tokens.t_in }
| "end"           { Tokens.t_end }
| "function"      { Tokens.t_function }
| "var"           { Tokens.t_var }
| "type"          { Tokens.t_type }
| "array"         { Tokens.t_array }
| "if"            { Tokens.t_if }
| "then"          { Tokens.t_then }
| "else"          { Tokens.t_else }
| "do"            { Tokens.t_do }
| "of"            { Tokens.t_of }
| "nil"           { Tokens.t_nil }
(* Punctuation *)
| ","             { Tokens.t_comma }
| ":"             { Tokens.t_colon }
| ";"             { Tokens.t_semicol }
| "("             { Tokens.t_lparen }
| ")"             { Tokens.t_rparen }
| "["             { Tokens.t_lsquareb }
| "]"             { Tokens.t_rsquareb }
| "{"             { Tokens.t_lcurlyb }
| "}"             { Tokens.t_rcurlyb }
| "."             { Tokens.t_dot }
| "+"             { Tokens.t_plus }
| "-"             { Tokens.t_minus }
| "*"             { Tokens.t_mul }
| "/"             { Tokens.t_div }
| "="             { Tokens.t_eq }
| "<>"            { Tokens.t_noteq }
| "<"             { Tokens.t_less }
| "<="            { Tokens.t_lesseq }
| ">"             { Tokens.t_greater }
| ">="            { Tokens.t_greatereq }
| "&"             { Tokens.t_and }
| "|"             { Tokens.t_or }
| ":="            { Tokens.t_assign }
(* Literals  *)
| ['0'-'9']+              as n { Tokens.t_num (int_of_string n) }
| ['0'-'9']+'.'['0'-'9']* as n { Tokens.t_real (float_of_string n) }
| ['0'-'9']*'.'['0'-'9']+ as n { Tokens.t_real (float_of_string n) }
(*| ['a'-'z']+       { Tokens.t_string "todo"}*)
(* Identifier *)
| ['a'-'z''A'-'Z']+['a'-'z''A'-'Z''0'-'9''_']* as id { Tokens.t_id id }
| eof              { raise Eof }
