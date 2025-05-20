open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result = 
  let (t,e) = parse_or toks in (t,e)

  and parse_or toks = let (t1, e1) = parse_and toks in match lookahead t1 with
    Tok_Or -> let t = match_token t1 Tok_Or in 
      let (t2, e2) = parse_or t in (t2, Or(e1, e2))
    |_ -> parse_and toks

  and parse_and toks = let (t1, e1) = parse_equal toks in match lookahead t1 with
    Tok_And -> let t = match_token t1 Tok_And in 
      let (t2, e2) = parse_and t in (t2, And(e1, e2))
    |_ -> parse_equal toks

  and parse_equal toks = let (t1, e1) = parse_rel toks in match lookahead t1 with
    Tok_Equal -> let t = match_token t1 Tok_Equal in 
      let (t2, e2) = parse_equal t in (t2, Equal(e1, e2))
    |Tok_NotEqual -> let t = match_token t1 Tok_NotEqual in 
      let (t2, e2) = parse_equal t in (t2, NotEqual(e1, e2))
    |_ -> parse_rel toks

  and parse_rel toks = let (t1, e1) = parse_add toks in match lookahead t1 with
    Tok_Less -> let t = match_token t1 Tok_Less in 
      let (t2, e2) = parse_rel t in (t2, Less(e1, e2))
    |Tok_Greater -> let t = match_token t1 Tok_Greater in 
      let (t2, e2) = parse_rel t in (t2, Greater(e1, e2))
    |Tok_LessEqual -> let t = match_token t1 Tok_LessEqual in 
      let (t2, e2) = parse_rel t in (t2, LessEqual(e1, e2))
    |Tok_GreaterEqual -> let t = match_token t1 Tok_GreaterEqual in 
      let (t2, e2) = parse_rel t in (t2, GreaterEqual(e1, e2))
    |_ -> parse_add toks

  and parse_add toks = let (t1, e1) = parse_mult toks in match lookahead t1 with
    Tok_Add -> let t = match_token t1 Tok_Add in 
      let (t2, e2) = parse_add t in (t2, Add(e1, e2))
    |Tok_Sub -> let t = match_token t1 Tok_Sub in 
      let (t2, e2) = parse_mult t in (t2, Sub(e1, e2))
    |_ -> parse_mult toks

  and parse_mult toks = let (t1, e1) = parse_pow toks in match lookahead t1 with
    Tok_Mult -> let t = match_token t1 Tok_Mult in 
      let (t2, e2) = parse_mult t in (t2, Mult(e1, e2))
    |Tok_Div -> let t = match_token t1 Tok_Div in 
      let (t2, e2) = parse_mult t in (t2, Div(e1, e2))
    |_ -> parse_pow toks

  and parse_pow toks = let (t1, e1) = parse_unary toks in match lookahead t1 with 
    Tok_Pow -> let t = match_token t1 Tok_Pow in 
      let (t2, e2) = parse_pow t in (t2, Pow (e1, e2))
    |_ -> parse_unary toks

  and parse_unary toks = match lookahead toks with
    Tok_Not -> let t = match_token toks Tok_Not in 
      let (t1, e1) = parse_unary t in (t1, Not e1)
    |_ -> parse_prim toks

  and parse_prim toks = match lookahead toks with
    Tok_Int n -> let t  = match_token toks (Tok_Int n) in (t, Int n)
    |Tok_Bool b -> let t = match_token toks (Tok_Bool b) in (t, Bool b)
    |Tok_ID s -> let t = match_token toks (Tok_ID s) in (t, ID s)
    |Tok_LParen -> let t = match_token toks Tok_LParen in
      let (t1, e1) = parse_expr t in if lookahead t1 = Tok_RParen then
        (match_token t1 Tok_RParen, e1) else raise (InvalidInputException "Missing Paren")
    |_ -> raise (InvalidInputException "Invalid Token") 

let rec parse_stmt toks : stmt_result = match lookahead toks with
  |Tok_Int_Type -> let (t, e) = parse_declare toks in let (t2, e2) = parse_stmt t in (t2, Seq(e, e2))
  |Tok_Bool_Type -> let (t, e) = parse_declare toks in let (t2, e2) = parse_stmt t in (t2, Seq(e, e2))
  |Tok_ID str -> let (t, e) = parse_assign toks in let (t2, e2) = parse_stmt t in (t2, Seq(e, e2))
  |Tok_Print -> let (t, e) = parse_print toks in let (t2, e2) = parse_stmt t in (t2, Seq(e, e2))
  |Tok_If -> let (t, e) = parse_if toks in let (t2, e2) = parse_stmt t in (t2, Seq(e, e2))
  |Tok_For -> let (t, e) = parse_for toks in let (t2, e2) = parse_stmt t in (t2, Seq(e, e2))
  |Tok_While -> let (t, e) = parse_while toks in let (t2, e2) = parse_stmt t in (t2, Seq(e, e2))
  |EOF -> ([EOF], NoOp)
  |_ -> (toks, NoOp)

  and parse_declare toks = match lookahead toks with
    |Tok_Int_Type -> let t = match_token toks Tok_Int_Type in (match lookahead t with
      |Tok_ID str -> let t2 = match_token t (Tok_ID str) in let t3 = match_token t2 Tok_Semi in
        (t3, Declare(Int_Type, str))
      |_ -> raise (InvalidInputException "Expected String in Int Declaration"))

    |Tok_Bool_Type -> let t = match_token toks Tok_Bool_Type in (match lookahead t with
      Tok_ID str -> let t2 = match_token t (Tok_ID str) in let t3 = match_token t2 Tok_Semi in
        (t3, Declare(Bool_Type, str))
      |_ -> raise (InvalidInputException "Expected String in Int Declaration"))
    |_ -> raise (InvalidInputException "Not a Declaration")

  and parse_assign toks = match lookahead toks with 
    Tok_ID str -> let t = match_token toks (Tok_ID str) in let t2 = match_token t Tok_Assign in
      let (t3, e3) = parse_expr t2 in let t4 = match_token t3 Tok_Semi in (t4, Assign(str, e3))
    |_ -> raise (InvalidInputException "Expected String in Assignment")

  and parse_print toks = let t = match_token (match_token toks Tok_Print) Tok_LParen in
      let(t2, e2) = parse_expr t in let t3 = match_token (match_token t2 Tok_RParen) Tok_Semi in
      (t3, Print e2)
  
  and parse_if toks = let t = match_token (match_token toks Tok_If) Tok_LParen in 
      let (t2, e2) = parse_expr t in let t3 = match_token (match_token t2 Tok_RParen) Tok_LBrace in
      let (t4, e4) = parse_stmt t3 in let t5 = match_token t4 Tok_RBrace in if (lookahead t5) = Tok_Else then
        let t6 = match_token (match_token t5 Tok_Else) Tok_LBrace in 
        let (t7, e7) = parse_stmt t6 in let t8 = match_token t7 Tok_RBrace in (t8, If(e2, e4, e7))
      else (t5, If(e2, e4, NoOp))

  and parse_for toks = let t = match_token (match_token toks Tok_For) Tok_LParen in (match lookahead t with
      Tok_ID str -> let t2 = match_token (match_token t (Tok_ID str)) Tok_From in
        let (t3, e3) = parse_expr t2 in let t4 = match_token t3 Tok_To in 
        let (t5, e5) = parse_expr t4 in let t6 = match_token (match_token t5 Tok_RParen) Tok_LBrace in
        let (t7, e7) = parse_stmt t6 in let t8 = match_token t7 Tok_RBrace in (t8, For(str, e3, e5, e7))
      |_ -> raise (InvalidInputException "Expected String in For"))

  and parse_while toks = let t = match_token toks Tok_While in let t2 = match_token t Tok_LParen in
      let (t3, e3) = parse_expr t2 in let t4 = match_token (match_token t3 Tok_RParen) Tok_LBrace in 
      let (t5, e5) = parse_stmt t4 in let t6 = match_token t5 Tok_RBrace in (t6, While(e3, e5))

let parse_main toks : stmt = let t = match_token (match_token (match_token (match_token 
  (match_token toks Tok_Int_Type) Tok_Main) Tok_LParen) Tok_RParen)Tok_LBrace  in 
  let (t2, e2) = parse_stmt t in let t3 = match_token t2 Tok_RBrace in if t3 = [EOF] then e2 else 
    raise (InvalidInputException "Invalid Main")