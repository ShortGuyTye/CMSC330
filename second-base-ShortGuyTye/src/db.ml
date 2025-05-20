type person = { name : string; age : int; hobbies : string list }
type comparator = person -> person -> int
type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

type db  = person list

let newDatabase = []
;;

let insert person db = person :: db
;;

let rec remove name db = match db with
  [] -> []
  |h::t -> if h.name = name then remove name t
  else h :: remove name t
;;

let sort comparator db = List.sort comparator db
;;

let rec query condition db = let rec test_person cond person = match cond with 
| True -> true
| False -> false
| Age(x) -> x person.age
| Name(x) -> x person.name
| Hobbies(x) -> x person.hobbies
| And(x, y) -> (test_person x person) && (test_person y person)
| Or (x, y) -> (test_person x person) || (test_person y person)
| Not(x) -> not (test_person x person)
| If (x, y, z) -> if test_person x person then test_person y person else test_person z person
in List.filter (test_person condition) db 
;;

let queryBy condition db comparator = sort comparator (query condition db)
;;

let update condition db change = List.map (fun ele -> if List.mem ele (query condition db) 
  then change ele else ele) db
;;


let rec deleteAll condition db = query (Not(condition)) db
;;
