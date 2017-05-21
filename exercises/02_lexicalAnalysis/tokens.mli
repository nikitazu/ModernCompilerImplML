type token

val t_if : token
val t_num : int -> token
val t_id : string -> token
val string_of_token : token -> string
