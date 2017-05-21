{
type pos = int
exception Eof
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| "if"            { Tokens.t_if }
| ['0'-'9']+      { Tokens.t_num 0 }
| ['a'-'z']+      { Tokens.t_id "todo" }
| eof             { raise Eof }
