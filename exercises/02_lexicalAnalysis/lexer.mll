{
type pos = int
type token = IF | NUM | ID
exception Eof
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| "if"            { IF }
| ['0'-'9']+      { NUM }
| ['a'-'z']+      { ID }
| eof             { raise Eof }
