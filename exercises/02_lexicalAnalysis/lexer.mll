{
  open Lexing
  exception Eof
  exception IllegalString of int * int * string
  
  let raise_illegal_string lexbuf =
    let p = Lexing.lexeme_start_p lexbuf in
    IllegalString (p.pos_lnum, p.pos_cnum - p.pos_bol, Lexing.lexeme lexbuf)
    |> raise
  
  let raise_eof () =
    raise Eof

  type nop = unit (* do not remove this line, or TextMate coloring will break *)
}

let re_ws      = [' ''\t']
let re_newline = '\n'
let re_digit   = ['0'-'9']
let re_num     = re_digit+
let re_real1   = re_digit+'.'re_digit*
let re_real2   = re_digit*'.'re_digit+
let re_dquot   = '"'
let re_char    = [^ '"' '\\']

rule token = parse
| re_ws           { token lexbuf }
| re_newline      { new_line lexbuf; token lexbuf }
(* Keywords *)
| "while"         { Tokens.t_while lexbuf }
| "for"           { Tokens.t_for lexbuf }
| "to"            { Tokens.t_to lexbuf }
| "break"         { Tokens.t_break lexbuf }
| "let"           { Tokens.t_let lexbuf }
| "in"            { Tokens.t_in lexbuf }
| "end"           { Tokens.t_end lexbuf }
| "function"      { Tokens.t_function lexbuf }
| "var"           { Tokens.t_var lexbuf }
| "type"          { Tokens.t_type lexbuf }
| "array"         { Tokens.t_array lexbuf }
| "if"            { Tokens.t_if lexbuf }
| "then"          { Tokens.t_then lexbuf }
| "else"          { Tokens.t_else lexbuf }
| "do"            { Tokens.t_do lexbuf }
| "of"            { Tokens.t_of lexbuf }
| "nil"           { Tokens.t_nil lexbuf }
(* Punctuation *)
| ","             { Tokens.t_comma lexbuf }
| ":"             { Tokens.t_colon lexbuf }
| ";"             { Tokens.t_semicol lexbuf }
| "("             { Tokens.t_lparen lexbuf }
| ")"             { Tokens.t_rparen lexbuf }
| "["             { Tokens.t_lsquareb lexbuf }
| "]"             { Tokens.t_rsquareb lexbuf }
| "{"             { Tokens.t_lcurlyb lexbuf }
| "}"             { Tokens.t_rcurlyb lexbuf }
| "."             { Tokens.t_dot lexbuf }
| "+"             { Tokens.t_plus lexbuf }
| "-"             { Tokens.t_minus lexbuf }
| "*"             { Tokens.t_mul lexbuf }
| "/"             { Tokens.t_div lexbuf }
| "="             { Tokens.t_eq lexbuf }
| "<>"            { Tokens.t_noteq lexbuf }
| "<"             { Tokens.t_less lexbuf }
| "<="            { Tokens.t_lesseq lexbuf }
| ">"             { Tokens.t_greater lexbuf }
| ">="            { Tokens.t_greatereq lexbuf }
| "&"             { Tokens.t_and lexbuf }
| "|"             { Tokens.t_or lexbuf }
| ":="            { Tokens.t_assign lexbuf }

(* Literals  *)

| re_num              as n
  { Tokens.t_num (int_of_string n) lexbuf }

| re_real1 as n
  { Tokens.t_real (float_of_string n) lexbuf }
| re_real2 as n
  { Tokens.t_real (float_of_string n) lexbuf }

| re_dquot
  { read_string (Buffer.create 256) lexbuf }

(* Identifier *)

| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id
  { Tokens.t_id id lexbuf }

(* Comments *)
| "/*"_*"*/"       { token lexbuf }

(* EOF *)
| eof              { raise_eof () }


and read_string buf = parse
| re_dquot
  { Tokens.t_string (Buffer.contents buf) lexbuf }

| "\\t" { Buffer.add_char buf '\t'; read_string buf lexbuf }
| "\\r" { Buffer.add_char buf '\r'; read_string buf lexbuf }
| "\\n" { Buffer.add_char buf '\n'; read_string buf lexbuf }
| "\\b" { Buffer.add_char buf '\b'; read_string buf lexbuf }
| "\\\"" { Buffer.add_char buf '\"'; read_string buf lexbuf }
| "\\\\" { Buffer.add_char buf '\\'; read_string buf lexbuf }

| re_char+
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    read_string buf lexbuf
  }

(* Illegal string *)
| _ { raise_illegal_string lexbuf }

(* EOF *)
| eof { raise_eof () }
