(* Source code example:
   a := 5+3; b := (print(a, a-1), 10*a); print(b)
*)

type id = string
type num = int

type bop =
| AddBop
| SubBop
| MulBop
| DivBop

type stm =
| CompoundStm of stm * stm
| AssignStm of id * exp
| PrintStm of exp list

and  exp =
| IdExp of id
| NumExp of num
| BopExp of exp * bop * exp
| SeqExp of stm * exp

module Env = struct
  type env = (id * num) list
  
  let put key value env =
    (key, value) :: env
  
  let get key env =
    List.assoc key env
end


let program =
  CompoundStm(
    AssignStm("a", BopExp(NumExp 5, AddBop, NumExp 3)),
    CompoundStm(
      AssignStm("b", 
        SeqExp(
          PrintStm [IdExp "a"; BopExp(IdExp "a", SubBop, NumExp 1)],
          BopExp(NumExp 10, MulBop, IdExp "a"))),
      PrintStm [IdExp "b"]))

(* Task 1 - maxargs *)
let rec maxargs stm =
  let max a b =
    if a > b then a else b in

  let rec maxargs_exp exp =
    match exp with
    | IdExp _ -> 0
    | NumExp _ -> 0
    | BopExp (e1, _, e2) -> max (maxargs_exp e1) (maxargs_exp e2)
    | SeqExp (s, e) -> max (maxargs_stm s) (maxargs_exp e)
  
  and maxargs_stm stm =
    match stm with
    | CompoundStm (s1, s2) -> max (maxargs_stm s1) (maxargs_stm s2)
    | AssignStm (_, e) -> maxargs_exp e
    | PrintStm es ->
      max
        (List.length es)
        (List.fold_left (fun a b -> a + maxargs_exp b) 0 es)
  in
    maxargs_stm stm

(* Task 2 - interp *)
let interp stm =
  let rec interp_stm stm env =
    match stm with
    | CompoundStm (s1, s2) -> interp_stm s1 env |> interp_stm s2
    | AssignStm (name, e) ->
      let (value, env2) = interp_exp e env in
      Env.put name value env2
    | PrintStm es ->
      let print_exp (i, acc_env) e =
        let (value, env2) = interp_exp e acc_env in
        Printf.fprintf
          stdout
          (if i > 0 then " %d" else "%d")
          value;
        (i+1, env2)
      in
        let (_, env2) = List.fold_left print_exp (0, env) es in
        Printf.fprintf stdout "\n";
        env2
    
  and interp_exp exp env =
    match exp with
    | IdExp name -> (Env.get name env, env)
    | NumExp value -> (value, env)
    | BopExp (e1, bop, e2) ->
      let (v1, env2) = interp_exp e1 env in
      let (v2, env3) = interp_exp e2 env2 in
      let v3 = 
        match bop with
        | AddBop -> v1 + v2
        | SubBop -> v1 - v2
        | MulBop -> v1 * v2
        | DivBop -> v1 / v2 in
      (v3, env3)
    | SeqExp (s, e) -> interp_stm s env |> interp_exp e
  in
    interp_stm stm []


let main _ =
  let argc = maxargs program in
  Printf.fprintf stdout "It works!!\n";
  Printf.fprintf stdout "maxargs = %d\n" argc;
  Printf.fprintf stdout "interpreting\n";
  interp program;
  Printf.fprintf stdout "interpreting done\n"

let () =
  main ()
