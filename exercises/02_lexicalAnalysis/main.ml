(* Only lexer is implemented.
   All tokens are printed.
 *)
let main _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
        Lexer.token lexbuf
        |> Tokens.string_of_token
        |> print_string
        |> print_newline;
        flush stdout
      done
  with Lexer.Eof ->
    exit 0

let () =
  main ()
