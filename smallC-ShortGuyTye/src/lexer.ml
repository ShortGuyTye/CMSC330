open TokenTypes

let re_for = Re.Str.regexp "for";;
let re_from = Re.Str.regexp "from";;
let re_to = Re.Str.regexp "to";;
let re_while = Re.Str.regexp "while";;
let re_int_type = Re.Str.regexp "int";;
let re_bool_type = Re.Str.regexp "bool";;
let re_sub = Re.Str.regexp "-";;
let re_semi = Re.Str.regexp ";";;
let re_rparen = Re.Str.regexp ")";;
let re_rbrace = Re.Str.regexp "}";;
let re_print = Re.Str.regexp "printf";;
let re_pow = Re.Str.regexp "\\^";;
let re_add = Re.Str.regexp "\\+";;
let re_or = Re.Str.regexp "||";;
let re_notequal = Re.Str.regexp "!=";;
let re_not = Re.Str.regexp "!";;
let re_mult = Re.Str.regexp "\\*";;
let re_main = Re.Str.regexp "main";;
let re_lessequal = Re.Str.regexp "<=";;
let re_less = Re.Str.regexp "<";;
let re_lparen = Re.Str.regexp "(";;
let re_lbrace = Re.Str.regexp "{";;
let re_int = Re.Str.regexp "-?[0-9]+";;
let re_if = Re.Str.regexp "if";;
let re_id = Re.Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let re_greaterequal = Re.Str.regexp ">=";;
let re_greater = Re.Str.regexp ">";;
let re_equal = Re.Str.regexp "==";;
let re_else = Re.Str.regexp "else";;
let re_div = Re.Str.regexp "/";;
let re_bool = Re.Str.regexp "true\\|false";;
let re_assign = Re.Str.regexp "=";;
let re_and = Re.Str.regexp "&&";;
let re_whitespace = Re.Str.regexp "[ \n\t]+";;

let tokenize input =
  let rec tok pos str = 
    if pos >= String.length str then [EOF]

    else if Re.Str.string_match re_for str pos && 
      (not(Re.Str.string_match re_id str (pos+3)) && not(Re.Str.string_match re_int str (pos+3))) then 
      Tok_For::(tok (pos+3) str)
    else if Re.Str.string_match re_from str pos && 
      (not(Re.Str.string_match re_id str (pos+4)) && not(Re.Str.string_match re_int str (pos+4))) then 
      Tok_From::(tok (pos+4) str)
    else if Re.Str.string_match re_to str pos && 
      (not(Re.Str.string_match re_id str (pos+2)) && not(Re.Str.string_match re_int str (pos+2))) then 
      Tok_To::(tok (pos+2) str)
    else if Re.Str.string_match re_while str pos && 
      (not(Re.Str.string_match re_id str (pos+5)) && not(Re.Str.string_match re_int str (pos+5))) then 
      Tok_While::(tok (pos+5) str)
    else if Re.Str.string_match re_int_type str pos && 
      (not(Re.Str.string_match re_id str (pos+3)) && not(Re.Str.string_match re_int str (pos+3))) then 
      Tok_Int_Type::(tok (pos+3) str)
    else if Re.Str.string_match re_bool_type str pos && 
      (not(Re.Str.string_match re_id str (pos+4)) && not(Re.Str.string_match re_int str (pos+4))) then 
      Tok_Bool_Type::(tok (pos+4) str)
    else if Re.Str.string_match re_semi str pos then 
      Tok_Semi::(tok (pos+1) str)
    else if Re.Str.string_match re_rparen str pos then 
      Tok_RParen::(tok (pos+1) str)
    else if Re.Str.string_match re_rbrace str pos then 
      Tok_RBrace::(tok (pos+1) str)
    else if Re.Str.string_match re_print str pos && 
      (not(Re.Str.string_match re_id str (pos+6)) && not(Re.Str.string_match re_int str (pos+6))) then 
      Tok_Print::(tok (pos+6) str)
    else if Re.Str.string_match re_pow str pos then 
      Tok_Pow::(tok (pos+1) str)
    else if Re.Str.string_match re_add str pos then 
      Tok_Add::(tok (pos+1) str)
    else if Re.Str.string_match re_or str pos then 
      Tok_Or::(tok (pos+2) str)
    else if Re.Str.string_match re_notequal str pos then 
      Tok_NotEqual::(tok (pos+2) str)
    else if Re.Str.string_match re_not str pos then 
      Tok_Not::(tok (pos+1) str)
    else if Re.Str.string_match re_mult str pos then 
      Tok_Mult::(tok (pos+1) str)
    else if Re.Str.string_match re_main str pos && 
      (not(Re.Str.string_match re_id str (pos+4)) && not(Re.Str.string_match re_int str (pos+4))) then 
      Tok_Main::(tok (pos+4) str)
    else if Re.Str.string_match re_lessequal str pos then 
      Tok_LessEqual::(tok (pos+2) str)
    else if Re.Str.string_match re_less str pos then 
      Tok_Less::(tok (pos+1) str)
    else if Re.Str.string_match re_lparen str pos then 
      Tok_LParen::(tok (pos+1) str)
    else if Re.Str.string_match re_lbrace str pos then 
      Tok_LBrace::(tok (pos+1) str)
    else if Re.Str.string_match re_if str pos && 
      (not(Re.Str.string_match re_id str (pos+2)) && not(Re.Str.string_match re_int str (pos+2))) then 
      Tok_If::(tok (pos+2) str)
    else if Re.Str.string_match re_greaterequal str pos then 
      Tok_GreaterEqual::(tok (pos+2) str)
    else if Re.Str.string_match re_greater str pos then 
      Tok_Greater::(tok (pos+1) str)
    else if Re.Str.string_match re_equal str pos then 
      Tok_Equal::(tok (pos+2) str)
    else if Re.Str.string_match re_else str pos && 
      (not(Re.Str.string_match re_id str (pos+4)) && not(Re.Str.string_match re_int str (pos+4))) then 
      Tok_Else::(tok (pos+4) str)
    else if Re.Str.string_match re_div str pos then 
      Tok_Div::(tok (pos+1) str)
    else if Re.Str.string_match re_assign str pos then 
      Tok_Assign::(tok (pos+1) str)
    else if Re.Str.string_match re_and str pos then 
      Tok_And::(tok (pos+3) str)
    else if Re.Str.string_match re_whitespace str pos then
      let token = Re.Str.matched_string str in 
      let len = String.length token in
      (tok (pos+len) str)
    else if Re.Str.string_match re_int str pos then 
      let token = Re.Str.matched_string str in 
      let len = String.length token in
      Tok_Int (Stdlib.int_of_string token)::(tok (pos+len) str)
    else if Re.Str.string_match re_sub str pos then 
      Tok_Sub::(tok (pos+1) str)
    else if Re.Str.string_match re_bool str pos then 
      let token = Re.Str.matched_string str in
      let len = String.length token in
      Tok_Bool (Stdlib.bool_of_string token)::(tok (pos+len) str)
    else if Re.Str.string_match re_id str pos then 
      let token = Re.Str.matched_string str in
      let len = String.length token in
      Tok_ID token::(tok (pos+len) str)

    else failwith "Invalid Token"
  in tok 0 input
;;