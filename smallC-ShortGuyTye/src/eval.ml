open SmallCTypes
open Utils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let is_int x = match x with Int_Val x -> x | _ -> raise (TypeError "Incompatible type");;
let is_bool x = match x with Bool_Val x -> x | _ -> raise (TypeError "Incompatible type");;

let rec eval_expr env t = match t with 
  |Int x -> Int_Val x
  |Bool b -> Bool_Val b
  |ID str -> if List.assoc_opt str env = None then raise (DeclareError "ID not defined") else
    (match (List.assoc str env) with Int_Val x -> Int_Val x | Bool_Val b -> Bool_Val b)
  |Add (x, y) -> (match eval_expr env x, eval_expr env y with (a, b) -> Int_Val ((is_int a) + (is_int b)))
  |Sub (x, y) -> (match eval_expr env x, eval_expr env y with (a, b) -> Int_Val ((is_int a) - (is_int b)))
  |Mult (x, y) -> (match eval_expr env x, eval_expr env y with (a, b) -> Int_Val ((is_int a) * (is_int b)))
  |Div (x, y) -> (match eval_expr env x, eval_expr env y with 
    (a, b) -> Int_Val ((is_int a) / (if (is_int b) = 0 then raise DivByZeroError else is_int b)))
  |Pow (x, y) -> (match eval_expr env x, eval_expr env y with 
    (a, b) -> Int_Val (Stdlib.truncate(Stdlib.float (is_int a) ** Stdlib.float (is_int b))))
  |Or (x, y) -> (match eval_expr env x, eval_expr env y with (a, b) -> 
    let a1 = is_bool a in let b1 = is_bool b in Bool_Val(a1 || b1))
  |And (x, y) -> (match eval_expr env x, eval_expr env y with (a, b) -> 
    let a1 = is_bool a in let b1 = is_bool b in Bool_Val(a1 && b1))
  |Not x -> (match eval_expr env x with a -> Bool_Val(not(is_bool a)))
  |Greater (x, y) -> (match eval_expr env x, eval_expr env y with (a, b) -> Bool_Val ((is_int a) > (is_int b)))
  |Less (x, y) -> (match eval_expr env x, eval_expr env y with (a, b) -> Bool_Val ((is_int a) < (is_int b)))
  |GreaterEqual (x, y) -> (match eval_expr env x, eval_expr env y with (a, b) -> Bool_Val ((is_int a) >= (is_int b)))
  |LessEqual (x, y) -> (match eval_expr env x, eval_expr env y with (a, b) -> Bool_Val ((is_int a) <= (is_int b)))
  |Equal (x, y) -> (match eval_expr env x, eval_expr env y with 
    (Int_Val a, Int_Val b) -> Bool_Val (a = b)
    |(Bool_Val a, Bool_Val b) -> Bool_Val (a = b)
    |(_, _) -> raise (TypeError "Incompatible type"))
  |NotEqual (x, y) -> (match eval_expr env x, eval_expr env y with 
    (Int_Val a, Int_Val b) -> Bool_Val (a != b)
    |(Bool_Val a, Bool_Val b) -> Bool_Val (a != b)
    |(_, _) -> raise (TypeError "Incompatible type"))

let rec eval_stmt env s = match s with 
  |NoOp -> env
  |Seq (a, b) -> eval_stmt (eval_stmt env a) b
  |Declare (tp, str) -> if List.assoc_opt str env = None then (match tp with 
    Int_Type -> (str, Int_Val 0) :: env
    |Bool_Type -> (str, Bool_Val false) :: env) else
    raise (DeclareError "Name already in use")
  |Assign (str, exp) -> if List.assoc_opt str env = None then raise (DeclareError "No such name found") else
    (match eval_expr env exp with 
      Int_Val num -> (match List.assoc str env with 
        Int_Val num -> (str, (eval_expr env exp)) :: env
        |Bool_Val b -> raise (TypeError "Mismatched types"))
      |Bool_Val b -> (match List.assoc str env with 
        Int_Val num -> raise (TypeError "Mismatched types")
        |Bool_Val b -> (str, (eval_expr env exp)) :: env))
  |If (exp, s1, s2) -> (match eval_expr env exp with 
    Int_Val num -> raise (TypeError "If expecting boolean") 
    |Bool_Val b -> if b then eval_stmt env s1 else eval_stmt env s2)
  |While (exp, stm) -> (match eval_expr env exp with 
    Int_Val num -> raise (TypeError "While expecting boolean") 
    |Bool_Val b -> if b then eval_stmt (eval_stmt env stm) s else env)
  |For (str, e1, e2, stm) -> (match eval_expr env e1,eval_expr env e2 with 
    Int_Val num1, Int_Val num2 -> let newenv = if (List.assoc str env) < Int_Val num1 then 
      (str, Int_Val num1)::env else env in 
      if (List.assoc str newenv) <= Int_Val num2 then 
      eval_stmt (eval_stmt ((str, Int_Val(match List.assoc str newenv with 
        Int_Val x -> x + 1 |_-> failwith"how"))::newenv) stm) s else newenv
    |_ -> raise (TypeError "For expecting int"))
  |Print exp -> (match eval_expr env exp with 
    Int_Val num -> let _ = print_output_newline (print_output_int num) in env
    |Bool_Val b -> let _ = print_output_newline (print_output_bool b) in env)