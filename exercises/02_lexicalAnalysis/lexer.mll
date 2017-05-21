{
type pos = int
exception Eof
}

rule token = parse
| [' ' '\t' '\n']  { token lexbuf }
(* Keywords *)
| "while"          { Tokens.t_while }
| "for"            { Tokens.t_for }
| "to"             { Tokens.t_to }
| "break"          { Tokens.t_break }
| "let"            { Tokens.t_let }
| "in"             { Tokens.t_in }
| "end"            { Tokens.t_end }
| "function"       { Tokens.t_function }
| "var"            { Tokens.t_var }
| "type"           { Tokens.t_type }
| "array"          { Tokens.t_array }
| "if"             { Tokens.t_if }
| "then"           { Tokens.t_then }
| "else"           { Tokens.t_else }
| "do"             { Tokens.t_do }
| "of"             { Tokens.t_of }
| "nil"            { Tokens.t_nil }
(* Punctuation *)
| "comma"          { Tokens.t_comma }
| "column"         { Tokens.t_column }
| "semicol"        { Tokens.t_semicol }
| "lparen"         { Tokens.t_lparen }
| "rparen"         { Tokens.t_rparen }
| "lsquareb"       { Tokens.t_lsquareb }
| "rsquareb"       { Tokens.t_rsquareb }
| "lcurlyb"        { Tokens.t_lcurlyb }
| "rcurlyb"        { Tokens.t_rcurlyb }
| "dot"            { Tokens.t_dot }
| "plus"           { Tokens.t_plus }
| "minus"          { Tokens.t_minus }
| "mul"            { Tokens.t_mul }
| "div"            { Tokens.t_div }
| "eq"             { Tokens.t_eq }
| "noteq"          { Tokens.t_noteq }
| "less"           { Tokens.t_less }
| "lesseq"         { Tokens.t_lesseq }
| "greater"        { Tokens.t_greater }
| "greatereq"      { Tokens.t_greatereq }
| "and"            { Tokens.t_and }
| "or"             { Tokens.t_or }
| "assign"         { Tokens.t_assign }
(* Literals  *)
| ['0'-'9']+              as n { Tokens.t_num (int_of_string n) }
| ['0'-'9']+'.'['0'-'9']* as n { Tokens.t_real (float_of_string n) }
| ['0'-'9']*'.'['0'-'9']+ as n { Tokens.t_real (float_of_string n) }
(*| ['a'-'z']+       { Tokens.t_string "todo"}*)
(* Identifier *)
| ['a'-'z']+       { Tokens.t_id "todo" }
| eof              { raise Eof }
