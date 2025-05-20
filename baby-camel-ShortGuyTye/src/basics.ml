open Funs 

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let abs x = 
    if x < 0 then x * -1
    else x
;;

let rev_tup tup = 
    match tup with
    (a, b, c) -> (c, b, a)
;;

let is_even x = 
    if x mod 2 == 0 then true else false
;;

let area point1 point2 = 
    match (point1, point2) with
    (a, b), (c, d) -> let w = abs (c - a) in
    let l = abs(d - b) in w * l
;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = if n = 0 then 0 
    else if n = 1 then 1 
    else fibonacci (n-1)  + fibonacci (n-2)
;;

let rec pow x p = if p = 0 then 1 
    else if p = 1 then x 
    else x * pow x (p-1)
;;

let rec log x y = if y / x < 1 then 0
    else if y / x = 1 then 1
    else 1 + log x (y/x)
;;

let rec gcf x y = if x == 0 || y == 0 then x 
    else if x mod y == 0 then y
    else gcf y (x mod y)
;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec reverse_helper lst nlst = match lst with
    [] -> nlst
    |h::t -> reverse_helper t (h::nlst)
;;

let reverse lst =
    reverse_helper lst []
;;

let rec zip lst1 lst2 = match (lst1, lst2) with
    [], []-> []
    |[], _ -> []
    |_, [] -> []
    | h1::t1, h2::t2 -> match (h1, h2) with
        (x, y), (a, b) -> (x, y, a, b)::zip t1 t2
;;

let rec merge lst1 lst2 = match (lst1, lst2) with
    [], []-> []
    |[], h2::t2 -> h2::merge [] t2
    |h1::t1, [] -> h1::merge t1 []
    |h1::t1, h2::t2 -> if h1 < h2 then h1::merge t1 lst2
    else h2::merge lst1 t2
;;

let rec is_present lst v = match lst with
    [] -> false
    |h::t -> if h == v then true
    else is_present t v

let rec every_nth_helper n lst i = match lst with
        [] -> []
        |h::t -> if i mod n == 0 then h::every_nth_helper n t (i+1)
        else every_nth_helper n t (i+1)

let every_nth n lst = every_nth_helper n lst 1
;;

let rec jumping_tuples_helper lst1 lst2 i = match lst1, lst2 with
    [], [] -> []
    |h1::t1, h2::t2 -> if i mod 2 == 0 then match h2 with
        (x, y) -> y::jumping_tuples_helper t1 t2 (i+1)
    else match h1 with
        (x, y) -> x::jumping_tuples_helper t1 t2 (i+1) 
      
let jumping_tuples lst1 lst2 =  
    (jumping_tuples_helper lst1 lst2 0) @
    (jumping_tuples_helper lst1 lst2 1)
;;


let rec max_func_chain init funcs = match funcs with
    [] -> init
    |h::t -> if  max_func_chain init t > max_func_chain (h init) t then
        max_func_chain init t else max_func_chain (h init) t
;;