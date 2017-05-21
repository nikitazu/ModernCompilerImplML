type token =
| IF
| NUM of int
| ID of string

let t_if = IF
let t_num n = NUM n
let t_id id = ID id

let string_of_token = function
  | IF    -> "IF"
  | NUM n -> "NUM=" ^ string_of_int n
  | ID id -> "ID=" ^ id
