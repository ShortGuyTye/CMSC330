open List
open Nfa
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference:

  type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t
*)

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

let rec reg_help regexp nfa = let num = fresh() in let num2 = fresh() in match regexp with
    Empty_String -> {sigma = []; qs = [num]; q0 = num; fs = [num]; delta = []}
    |Char(x) -> {sigma = [x]; qs = [num;num2]; q0 = num; fs = [num2];
      delta = [{input = Some x; states = (num,num2)}]}
    |Union(x,y) -> let left = reg_help x nfa in let right = reg_help y nfa in
      {sigma = union left.sigma right.sigma; qs = union [num; num2] (union left.qs right.qs);
        q0 = num; fs = [num2]; delta = [{input = None; states = (num, left.q0)}] @ 
        [{input = None; states = (num, right.q0)}] @ [{input = None; states = (List.hd left.fs, num2)}] @
        [{input = None; states = (List.hd right.fs, num2)}] @ (union left.delta right.delta)}
    |Concat(x,y) -> let left = reg_help x nfa in let right = reg_help y nfa in
      {sigma = union left.sigma right.sigma; qs = union left.qs right.qs; q0 = left.q0; fs = right.fs;
      delta = [{input = None; states = (List.hd left.fs, right.q0)}] @ (union left.delta right.delta)}
    |Star(x) -> let left = reg_help x nfa in {sigma = left.sigma; qs = union [num; num2] left.qs; q0 = num; fs = [num2];
      delta = [{input = None; states = (num, num2)}] @ [{input = None; states = (num, left.q0)}] @
      [{input = None; states = (List.hd left.fs, num2)}] @ [{input = None; states = (num2, num)}] @ left.delta}
;;

let regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t = 
  reg_help regexp {sigma = []; qs = []; q0 = (-1); fs = []; delta = []}
;;

(* The following functions are useful for testing, we have implemented them for you *)
let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str