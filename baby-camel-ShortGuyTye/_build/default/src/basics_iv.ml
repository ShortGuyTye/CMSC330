open Funs
(*****************)
(* Part 4: HOF *)
(*****************)

let is_there lst x = if fold (fun acc ele -> if x == ele then acc + 1 else acc) 0 lst 
  > 0 then true else false
;;

let count_occ lst target = fold (fun acc ele -> if target == ele then acc + 1 else acc) 0 lst 
;;

let uniq lst = fold (fun acc ele -> if (is_there acc ele) then acc else ele::acc) [] lst
;;

let every_xth x lst = let answer = 
  fold (fun acc ele -> match acc with 
  (a, b) -> if b mod x == 0 then (a @ [ele], b + 1)
  else (a, b + 1)) ([], 1) lst in 
  match answer with (c, d) -> c
;;
