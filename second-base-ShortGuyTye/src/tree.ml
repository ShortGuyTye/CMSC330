type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec tree_fold f init tree = match tree with 
  | Leaf -> init
  | Node(l, v, r) -> f (tree_fold f init l) v (tree_fold f init r) 
;;

let map tree f = tree_fold (fun l v r -> Node(l, f v, r)) Leaf tree
;;
let mirror tree = tree_fold (fun l v r -> Node(r, v, l)) Leaf tree
;;
let in_order tree = tree_fold (fun l v r -> l @ v :: r ) [] tree
;;
let pre_order tree = tree_fold (fun l v r -> v :: l @ r ) [] tree
;;
let compose tree = tree_fold (fun l v r -> fun x -> r (v (l x))) (fun x -> x)  tree

let depth tree = tree_fold (fun l v r -> if l > r then l + 1 else r + 1) 0 tree

(* Assume complete tree *)

let trim tree n = match (tree_fold (fun l v r -> match l with
  (x, y) -> if (depth tree) - y > n then (Leaf, y + 1) else match r with 
    (a, b) -> (Node(x, v, a), y + 1)) (Leaf, 0) tree ) with
      (node, depth) -> node
;;

(*let rec tree_init f v = if (f v) = None then Leaf else match (f v) with 
  (l, n, r) -> Node(tree_init f l, n, tree_init f r)
;;*)

let rec split lst v = match lst with 
  [] -> ([], [])
  | h::t -> if h = v then ([], t) else match (split t v) with 
    (x, y) -> (h :: x, y)
;;

let rec from_pre_in pre in_ord = let rec find tree v = match tree with
| Leaf -> false
| Node(l, n, r) -> if n = v then true else
  if find l v  then true else  if find r v then true else false
  in let rec fix_list lst tree = match lst with
  [] -> []
  | h::t -> if (find tree h) = false then h::t else fix_list t tree
  in match pre, in_ord with 
    [], [] -> Leaf
    | [], _::_ -> Leaf
    | _::_, [] -> Leaf
    | ph::pt, ih::it -> match (split in_ord ph) with 
      (x, y) -> if (ph = ih) && x = [] && y = [] then Node(Leaf, ih, Leaf) else 
        Node (from_pre_in pt x, ph, from_pre_in (fix_list pt (from_pre_in pt x)) y)
;;
