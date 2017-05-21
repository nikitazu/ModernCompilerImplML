(* Only lexer is implemented.
   All tokens are printed.
 *)

let string_of_token = function
  | Lexer.IF  -> "IF"
  | Lexer.NUM -> "NUM"
  | Lexer.ID  -> "ID"

let main _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
        Lexer.token lexbuf
        |> string_of_token
        |> print_string
        |> print_newline;
        flush stdout
      done
  with Lexer.Eof ->
    exit 0

let () =
  main ()
