open List
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference

  type ('q, 's) transition = {
    input: 's option; 
    states: 'q * 'q;
  }

  (** NFA type *)
  type ('q, 's) nfa_t = {
    sigma: 's list;
    qs: 'q list;
    q0: 'q;
    fs: 'q list;
    delta: ('q, 's) transition list;
  } 
    let nfa_ex = {
    sigma = ['a'];
    qs = [0; 1; 2; 3 ;4];
    q0 = 0;
    fs = [2];
    delta = [{input = Some 'a'; states = (0, 1)}; {input = None; states = (1, 2)}; {input = None; states = (2, 3)}; {input = None; states = (3, 4)}]
    };;
*)

(****************)
(* Part 1: NFAs *)
(****************)
let rec move_helper nfa qs s = match qs with
  [] -> []
  | h::t -> (List.fold_left (fun acc ele -> match ele.states with (x, y) -> 
    if ele.input = s && x = h then y::acc else acc) [] (nfa.delta)) @ (move_helper nfa t s)
;;

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = match s with
  None -> List.sort_uniq Stdlib.compare (move_helper nfa qs s)
  | Some a -> if List.exists (fun x -> x = a) (nfa.sigma) then
    List.sort_uniq Stdlib.compare (move_helper nfa qs s) else []
  ;;

let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = let first = move nfa qs None in
    if eq (union qs first) qs then qs else e_closure nfa (union qs first)
;;

let rec includes list states = match list with
  [] -> false
  | h::t -> if elem h states then true else false || includes t states
;;

let rec accept_helper (nfa: ('q,char) nfa_t) path starts = match path with
  [] -> if includes nfa.fs (e_closure nfa starts) then true else false
  | h::t -> accept_helper nfa t (List.sort_uniq Stdlib.compare (move nfa (e_closure nfa starts) (Some h))) 
;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = accept_helper nfa (explode s) (e_closure nfa [nfa.q0])

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let rec new_states_helper (nfa: ('q,'s) nfa_t) qs alph = match alph with
[] -> []
| h::t -> let letter = move nfa qs (Some h) in 
    let epsilon = e_closure nfa letter in 
    [List.sort_uniq Stdlib.compare (letter @ epsilon)] @ (new_states_helper nfa qs t)
;;

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = new_states_helper nfa qs nfa.sigma

let rec new_trans_helper qs new_state alph = match new_state with
  [] -> []
  | h::t -> match alph with 
    [] -> []
    | h2::t2 -> [{input = Some h2; states = (qs, h)}] @ new_trans_helper qs t t2
;;
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list = 
  new_trans_helper qs (new_states nfa qs) nfa.sigma
;;

let rec new_finals_helper nfa qs tail = match tail with
  [] -> []
  | h::t -> if elem h nfa.fs then [qs] else new_finals_helper nfa qs t
;;

let rec new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = new_finals_helper nfa qs qs
;;

let rec nfa_to_dfa_garbage qs = match qs with 
  [] -> []
  | h::t -> match h.states with
    (x, y) ->  if y = [] then nfa_to_dfa_garbage t else
      h :: nfa_to_dfa_garbage t
;;

let rec nfa_to_dfa_step work nfa dfa = match work with
  [] -> dfa
  | h::t -> if h = [] then nfa_to_dfa_step t nfa dfa else let states = new_states nfa h in 
    nfa_to_dfa_step (union (diff states dfa.qs) (remove h work)) nfa
    ({dfa with qs = union dfa.qs states; delta = union dfa.delta (nfa_to_dfa_garbage (new_trans nfa h)); 
    fs = union (new_finals nfa h) dfa.fs})
;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = let e = e_closure nfa [nfa.q0] in
  let dfa = {sigma = nfa.sigma; qs = [e]; q0 = e; fs = new_finals nfa e; 
  delta = nfa_to_dfa_garbage (new_trans nfa e)} in nfa_to_dfa_step [e] nfa dfa 
;;