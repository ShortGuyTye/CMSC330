open Ast
open Utils

let rec reorder e = match e with 
  |Binop (op, e1, e2) -> (match e1 with 
    |Int x -> Binop (op, e1, (match e2 with Binop (op2, e3, e4) -> if op2 = op then reorder e2 else e2 |_ -> e2))
    |Value -> (match e2 with 
      |Int x -> Binop (op, e2, e1) 
      |Binop (op2, e3, e4) -> if op2 = op then (match reorder e2 with
        |Binop (op3, e5, e6) -> Binop (op, e5, reorder (Binop (op3, e1, e6)))
        |_ -> failwith "Reorder fail") else Binop(op, e1, e2) 
      |_ -> Binop (op, e1, e2))
    |Binop (op2, e3, e4) -> (match e2 with 
      |Binop _ -> Binop (op, (if op = op2 then reorder e1 else e1), (if op2 = op then reorder e2 else e2))
      |_ -> if e2 = Value then (match reorder e1 with 
        |Binop (nop2, ne3, ne4) -> Binop (op, ne3, reorder (Binop (nop2, e2, ne4)))
        |_ -> failwith "no")
        else Binop (op, e2, if op = op2 then reorder e1 else e1))
    |_ -> failwith "Not Int")
  |_ -> failwith "Not a Binop"

let rec optimize_exp e env = match e with 
  |Int x -> Int x
  |Bool b -> Bool b
  |ID str -> if List.assoc_opt str env = None then raise (DeclareError "No such variable") else 
    (match List.assoc str env with 
      |Int x -> Int x
      |Bool b -> Bool b
      |Value -> Value
      |_ -> ID str)
  |Binop (op, e1, e2) -> (match op with
    |Add -> if (num_check e1 env && num_check e2 env) then Int (int_of_exp e1 env + int_of_exp e2 env) else
      (match reorder e with 
        |Binop (op2, e3, e4) -> let fail = Binop (Add, optimize_exp e3 env , optimize_exp e4 env) in
          if num_check e3 env then (match e4 with 
            |Binop (op3, e5, e6) -> if num_check e5 env then 
              optimize_exp (Binop (Add, Int (int_of_exp e3 env + int_of_exp e5 env), e6)) env else 
                fail |_ -> fail) else fail |_ -> failwith "reorder fail")
    |Sub -> if num_check e1 env && num_check e2 env then Int (int_of_exp e1 env - int_of_exp e2 env) else 
      Binop(Sub, optimize_exp e1 env, optimize_exp e2 env)
    |Mult -> if num_check e1 env && num_check e2 env then Int (int_of_exp e1 env * int_of_exp e2 env) else 
      if (num_check e1 env && int_of_exp e1 env = 0) || (num_check e2 env && int_of_exp e2 env = 0) then Int 0 else 
        Binop(Mult, optimize_exp e1 env, optimize_exp e2 env)
    |Div -> if num_check e2 env && int_of_exp e2 env = 0 then raise (DivByZeroError) else
      if num_check e1 env && num_check e2 env then Int (int_of_exp e1 env / int_of_exp e2 env) else 
      Binop(Div, optimize_exp e1 env, optimize_exp e2 env)
    |Pow -> if num_check e1 env && num_check e2 env then Int (int_of_float (float_of_int (int_of_exp e1 env) ** 
      float_of_int(int_of_exp e2 env))) else Binop(Pow, optimize_exp e1 env, optimize_exp e2 env)
    |Greater -> if num_check e1 env && num_check e2 env then Bool (int_of_exp e1 env > int_of_exp e2 env) else 
      Binop(Greater, optimize_exp e1 env, optimize_exp e2 env)
    |Less -> if num_check e1 env && num_check e2 env then Bool (int_of_exp e1 env < int_of_exp e2 env) else 
      Binop(Less, optimize_exp e1 env, optimize_exp e2 env)
    |GreaterEqual -> if num_check e1 env && num_check e2 env then Bool (int_of_exp e1 env >= int_of_exp e2 env) else 
      Binop(GreaterEqual, optimize_exp e1 env, optimize_exp e2 env)
    |LessEqual -> if num_check e1 env && num_check e2 env then Bool (int_of_exp e1 env <= int_of_exp e2 env) else 
      Binop(LessEqual, optimize_exp e1 env, optimize_exp e2 env)
    |Equal -> if num_check e1 env && num_check e2 env then Bool (int_of_exp e1 env = int_of_exp e2 env) else
      if bool_check e1 env && bool_check e2 env then Bool(bool_of_exp e1 env = bool_of_exp e2 env) else
      Binop(Equal, optimize_exp e1 env, optimize_exp e2 env)
    |NotEqual -> if num_check e1 env && num_check e2 env then Bool (int_of_exp e1 env != int_of_exp e2 env) else
      if bool_check e1 env && bool_check e2 env then Bool(bool_of_exp e1 env != bool_of_exp e2 env) else
      Binop(NotEqual, optimize_exp e1 env, optimize_exp e2 env)
    |Or -> if bool_check e1 env && bool_check e2 env then Bool (bool_of_exp e1 env || bool_of_exp e2 env) else
      if (bool_check e1 env && bool_of_exp e1 env) || (bool_check e2 env && bool_of_exp e2 env) then Bool true else
      Binop(Or, optimize_exp e1 env, optimize_exp e2 env)
    |And -> if bool_check e1 env && bool_check e2 env then Bool (bool_of_exp e1 env && bool_of_exp e2 env) else
      if (bool_check e1 env && bool_of_exp e1 env = false) || (bool_check e2 env && bool_of_exp e2 env = false) then Bool false else
        Binop(And, optimize_exp e1 env, optimize_exp e2 env))
  |Not e -> (match optimize_exp e env with 
    |Bool b -> if b then Bool false else Bool true 
    |Not e2 -> optimize_exp e2 env
    |_ -> Not(optimize_exp e env))
  |Value -> Value

and num_check e  env = match optimize_exp e env with Int x -> true |_ -> false
and bool_check e env = match optimize_exp e env with Bool x -> true |_ -> false
and int_of_exp e env = match optimize_exp e env with Int x -> x |_ -> failwith "Not an Int"
and bool_of_exp e env = match optimize_exp e env with Bool x -> x |_ -> failwith "Not a Bool"

let rec optimize_stmt e env = match e with
  |NoOp -> NoOp
  |Seq (s1, s2) -> Seq (optimize_stmt s1 env, (match s1 with 
    |Assign (str, t, e) -> optimize_stmt s2 (match optimize_exp e env with 
      |Int x -> (str, Int x)::env 
      |Bool b -> (str, Bool b)::env 
      |Value -> (str, Value)::env
      |_ -> env)
    |_ -> optimize_stmt s2 env))
  |Assign (str, t, e) -> Assign (str, t, optimize_exp e (match optimize_exp e env with 
    |Int x -> (str, Int x)::env 
    |Bool b -> (str, Bool b)::env 
    |Value -> (str, Value)::env
    |_ -> env))
  |If (e, s1, s2) -> if optimize_stmt s1 env = optimize_stmt s2 env then s1 else (match optimize_exp e env with
    |Bool b -> if b then optimize_stmt s1 env else optimize_stmt s2 env
    |_ -> If (optimize_exp e env, optimize_stmt s1 env, optimize_stmt s2 env))
  |For (str, e1, e2, s) -> (match optimize_exp e1 env, optimize_exp e2 env with
    |Int x1, Int x2 -> if x1 > x2 then Assign(str, Int_Type, optimize_exp e1 env) else 
      For (str, optimize_exp e1 env, optimize_exp e2 env, optimize_stmt s env)
    |_, _ -> failwith "Invalid for")
  |While (e, s) -> (match optimize_exp e env with
    |Bool b -> if b then While (optimize_exp e env, optimize_stmt s env) else NoOp
    |_ -> While (optimize_exp e env, optimize_stmt s env))
  |Print (e) -> Print (optimize_exp e env)
  |_ -> failwith "Not a statement"

let rec optimize e = optimize_stmt e []

let rec get_env e env = match e with 
  |Seq (s1, s2) -> get_env s2 (get_env s1 env)
  |Assign (str, t, e) -> (str, match t with
    |Int_Type -> Int 0
    |Bool_Type -> Bool false
    |Unknown_Type 0 -> Value 
    |_ -> failwith "get_env")::env
  |_ -> env

let rec typecheck_exp e env = match e with
  |Int x -> Int_Type
  |Bool b -> Bool_Type
  |ID str -> if List.assoc_opt str env = None then raise (DeclareError "No such variable") else 
    (match List.assoc str env with 
      |Int x -> Int_Type
      |Bool b -> Bool_Type
      |Value -> Unknown_Type 0
      |_ -> raise (TypeError "idk how this could happen"))
  |Binop (op, e1, e2) -> (match op with
    |Add -> if (is_int e1 env) && (is_int e2 env) then Int_Type else raise (TypeError "Add expected Int")
    |Sub -> if (is_int e1 env) && (is_int e2 env) then Int_Type else raise (TypeError "Sub expected Int")
    |Mult -> if (is_int e1 env) && (is_int e2 env) then Int_Type else raise (TypeError "Mult expected Int")
    |Div -> if (is_int e1 env) && (is_int e2 env) then Int_Type else raise (TypeError "Div expected Int")
    |Pow -> if (is_int e1 env) && (is_int e2 env) then Int_Type else raise (TypeError "Pow expected Int")
    |Greater -> if ((is_int e1 env) && (is_int e2 env)) || ((is_bool e1 env) && (is_bool e2 env)) then 
      Bool_Type else raise (TypeError "Greater expected Int")
    |Less -> if ((is_int e1 env) && (is_int e2 env)) || ((is_bool e1 env) && (is_bool e2 env)) then 
      Bool_Type else raise (TypeError "Less expected Int")
    |GreaterEqual -> if ((is_int e1 env) && (is_int e2 env)) || ((is_bool e1 env) && (is_bool e2 env)) then 
      Bool_Type else raise (TypeError "GreaterEqual expected Int")
    |LessEqual -> if ((is_int e1 env) && (is_int e2 env)) || ((is_bool e1 env) && (is_bool e2 env)) then 
      Bool_Type else raise (TypeError "LessEqual expected Int")
    |Equal -> if ((is_int e1 env) && (is_int e2 env)) || ((is_bool e1 env) && (is_bool e2 env)) then 
      Bool_Type else raise (TypeError "Equal expected Bool or Int")
    |NotEqual -> if ((is_int e1 env) && (is_int e2 env)) || ((is_bool e1 env) && (is_bool e2 env)) then 
      Bool_Type else raise (TypeError "NotEqual expected Bool or Int")
    |Or -> if (is_bool e1 env) && (is_bool e2 env) then Bool_Type else raise (TypeError "Or expected Bool")
    |And -> if (is_bool e1 env) && (is_bool e2 env) then Bool_Type else raise (TypeError "And expected Bool"))
  |Not e -> (match typecheck_exp e env with Bool_Type -> Bool_Type 
    |Unknown_Type x -> Unknown_Type x |_ -> raise (TypeError "Not expected Bool"))
  |Value -> Unknown_Type 0

and is_int e env = match typecheck_exp e env with Int_Type -> true |Unknown_Type x -> true |_ -> false
and is_bool e env = match typecheck_exp e env with Bool_Type -> true |Unknown_Type x -> true |_ -> false

let rec typecheck_stmt e env = match e with
  |NoOp -> true
  |Seq (s1, s2) -> if typecheck_stmt s1 env && typecheck_stmt s2 (get_env s1 env) then 
    true else raise (TypeError "Invalid Types")
  |Assign (str, t, e) -> if (List.assoc_opt str env = None) then 
    (match typecheck_exp e env with 
      |Int_Type -> (match t with 
        |Int_Type -> true
        |Unknown_Type x -> true
        |_ -> raise (TypeError "Bad Assignment"))
      |Bool_Type -> (match t with
        |Bool_Type -> true 
        |Unknown_Type x -> true
        |_ -> raise (TypeError "Bad Assignment"))
      |Unknown_Type x -> true) 
    else (match List.assoc str env with
      |Int x -> (match typecheck_exp e env with Int_Type -> true |Unknown_Type x -> true |_ -> raise (TypeError "Invalid Assignment 1"))
      |Bool x -> (match typecheck_exp e env with Bool_Type -> true |Unknown_Type x -> true |_ -> raise (TypeError "Invalid Assignment 2"))
      |_ -> true)
  |If (e, s1, s2) -> if (match typecheck_exp e env with Bool_Type -> true |Unknown_Type x -> true|_ -> false) && 
    (typecheck_stmt s1 env) && (typecheck_stmt s2 env) then true else raise (TypeError "Invalid Types")        
  |For (str, e1, e2, s) -> if List.assoc_opt str env = None then raise (DeclareError "For varible") else if
      (match List.assoc str env with Int x -> true |Value -> true |_ -> raise (TypeError "For needs Int")) &&
      ((match typecheck_exp e1 env with Bool_Type -> false |_ -> true) && 
        (match typecheck_exp e2 env with Bool_Type -> false |_ -> true)) then typecheck_stmt s env else 
        raise (TypeError "For expects var 1")
  |While (e, s) -> if (match typecheck_exp e env with Bool_Type -> true |Unknown_Type x -> true|_ -> false) && 
    typecheck_stmt s env then true else raise (TypeError "Invalid Type")
  |Print e -> match typecheck_exp e env with _ -> true

let rec typecheck e = typecheck_stmt e []

let rec infer_exp e env = match e with
  |Binop (op, e1, e2) -> (match op with
    |Add -> int_update e2 (int_update e1 env)
    |Sub -> int_update e2 (int_update e1 env)
    |Mult -> int_update e2 (int_update e1 env)
    |Div -> int_update e2 (int_update e1 env)
    |Pow -> int_update e2 (int_update e1 env)
    |Or -> bool_update e2 (bool_update e1 env)
    |And -> bool_update e2 (bool_update e1 env)
    |_ -> env)
  |Not e1 -> bool_update e1 env
  |_ -> env
    
and int_update e env = match e with ID str -> if List.assoc_opt str env = None then 
  raise (DeclareError "not declared") else (match List.assoc str env with 
    |Bool_Type -> raise (TypeError "Int of Bool") |_ -> (str, Int_Type)::env) |_ -> env
and bool_update e env =  match e with ID str -> if List.assoc_opt str env = None then 
  raise (DeclareError "not declared") else (match List.assoc str env with 
    |Int_Type -> raise (TypeError "Bool of Int") |_ -> (str, Bool_Type)::env) |_ -> env

let rec infer_stmt e env = match e with
  |NoOp -> env
  |Seq (s1, s2) -> infer_stmt s2 (infer_stmt s1 env)
  |Assign (str, t, e) -> (str, t)::(infer_exp e env)
  |If (e, s1, s2) -> infer_stmt s2 (infer_stmt s1 (match e with ID str -> (str, Bool_Type)::env |_ -> infer_exp e env))
  |For (str, e1, e2, s) -> if List.assoc_opt str env = None then raise (DeclareError "For expects int") else
    (match List.assoc str env with 
      |Int_Type -> infer_stmt s ((str, Int_Type)::(infer_exp e2 (infer_exp e1 env)))
      |Unknown_Type x -> infer_stmt s (infer_exp e2 (infer_exp e1 ((str, Int_Type)::env)))
      |_ -> raise (TypeError "For expected Int"))
  |While (e, s) -> infer_stmt s (match e with ID str -> (str, Bool_Type)::env |_ -> infer_exp e env)
  |Print e -> infer_exp e env

let rec infer_helper e env = match e with
  | NoOp -> NoOp
  | Seq (s1, s2) -> Seq (infer_helper s1 env, infer_helper s2 env)
  | Assign (str, t, e) -> Assign (str, List.assoc str env, e)
  | If (e, s1, s2) -> If (e, infer_helper s1 env, infer_helper s2 env)
  | For (str, e1, e2, s) -> For (str, e1, e2, infer_helper s env)
  | While (e, s) -> While (e, infer_helper s env)
  | Print e -> Print e

let rec infer e = let inf = infer_helper e (infer_stmt e []) in 
  if typecheck inf then inf else raise (TypeError "Invalid Type")

