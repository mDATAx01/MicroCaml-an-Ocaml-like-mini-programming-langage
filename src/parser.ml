open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  let (rem_toks, expr) = parse_Expr toks in
  if rem_toks <> [] then raise (InvalidInputException ":(")
  else (rem_toks, expr)


and parse_Expr toks =
  match lookahead toks with
  | Some Tok_Let -> parse_LetExpr toks
  | Some Tok_Fun -> parse_FunctionExpr toks
  | Some Tok_If -> parse_IfExpr toks
  | Some Tok_Not -> parse_OrExpr toks
  | Some (Tok_Int _) -> parse_OrExpr toks
  | Some (Tok_Bool _) -> parse_OrExpr toks
  | Some (Tok_String _) -> parse_OrExpr toks
  | Some (Tok_ID _) -> parse_OrExpr toks
  | Some Tok_LParen -> parse_OrExpr toks
  | Some Tok_LCurly -> parse_RecordExpr toks  (* New: Handle record expressions *)
  | _ -> raise (InvalidInputException ":(")

and parse_RecordExpr toks =
  let tok2 = match_token toks Tok_LCurly in
  let (toks3, fields) = parse_FieldList tok2 [] in
  let tok4 = match_token toks3 Tok_RCurly in
  (tok4, Record fields)

and parse_FieldList toks acc =
  match lookahead toks with
  | Some Tok_RCurly -> (toks, List.rev acc)
  | _ ->
    let (toks1, label) = parse_Label toks in
    let tok2 = match_token toks1 Tok_Equal in
    let (toks3, expr) = parse_Expr tok2 in
    let new_acc = (label, expr) :: acc in
    match lookahead toks3 with
    | Some Tok_DoubleSemi -> parse_FieldList (match_token toks3 Tok_DoubleSemi) new_acc
    | _ -> (toks3, List.rev new_acc)

and parse_Label toks =
  match lookahead toks with
  | Some (Tok_ID id) -> (match_token toks (Tok_ID id), Lab id)
  | _ -> raise (InvalidInputException "Invalid label")


and parse_LetExpr toks =
  let tok2 = match_token toks Tok_Let in                                 (* Consume 'let' token *)
  let (tok3, is_r) = parse_Recursion tok2 in                             (* Parse recursion keyword *)
  match lookahead tok3 with
  |Some (Tok_ID d) ->                                                    (* If the next token is an identifier *)
    let tok4 = match_token (match_token tok3 (Tok_ID d)) Tok_Equal in    (* Consume identifier and '=' token *)
    let (tok5, expr1) = parse_Expr tok4 in                               (* Parse the expression for the binding *)
    let (tok6, expr2) = parse_Expr (match_token tok5 Tok_In) in          (* Parse the body of the let *)
    (tok6, Let(d, is_r, expr1, expr2))                                   (* Return the let expression *)
  |_ -> raise (InvalidInputException ":(")                               (* Otherwise, raise an exception *)


and parse_Recursion toks =
  match lookahead toks with
  |Some Tok_Rec -> (match_token toks Tok_Rec, true)    (* If the next token is 'rec', consume it and set is_r to true *)
  |_ -> (toks, false)                                  (* Otherwise, set is_r to false *)


and parse_FunctionExpr toks =
  let tok2 = match_token toks Tok_Fun in                                (* Consume 'fun' token *)
  match lookahead tok2 with
  |Some (Tok_ID d) ->                                   
    let tok3 = match_token (match_token tok2 (Tok_ID d)) Tok_Arrow in   (* Consume identifier and '->' token *)
    let (tok4, expr) = parse_Expr tok3 in                               (* Parse the expression representing the function body *)
    (tok4, Fun(d, expr))                                                (* Return the function expression *)
  |_ -> raise (InvalidInputException ":(")                              (* Otherwise, raise an exception *)
 

and parse_IfExpr toks =
  let (tok2, expr2) = parse_Expr (match_token toks Tok_If) in    (* Consume 'if' token and parse condition expression *)
  let (tok3, expr3) = parse_Expr (match_token tok2 Tok_Then) in  (* And so on *)
  let (tok4, expr4) = parse_Expr (match_token tok3 Tok_Else) in  
  (tok4, If(expr2, expr3, expr4))                                (* Return the if expression *)


and parse_OrExpr toks =
  let (tok2, expr2) = parse_AndExpr toks in                  (* Parse the left side of the OR expression *)
  match lookahead tok2 with
  |Some Tok_Or ->                                            (* If the next token is 'or' *)
    let tok3 = match_token tok2 Tok_Or in                    (* Consume 'or' token *)
    let (tok4, expr4) = parse_OrExpr tok3 in                 (* Parse the right side of the OR expression *)
    (tok4, Binop(Or, expr2, expr4))                          (* Return the OR expression *)
  |_ -> (tok2, expr2)                                        (* If not, return the left side expression *)


and parse_AndExpr toks =
  let (tok2, expr2) = parse_EqualityExpr toks in            (* Parse the left side of the AND expression *)
  match lookahead tok2 with
  |Some Tok_And ->                                          
    let tok3 = match_token tok2 Tok_And in                  
    let (tok4, expr4) = parse_AndExpr tok3 in               
    (tok4, Binop(And, expr2, expr4))                        
  |_ -> (tok2, expr2)                                       


and parse_EqualityExpr toks =
  let (tok2, expr2) = parse_RelationalExpr toks in          (* Parse the left side of the equality expression *)
  match lookahead tok2 with
  |Some Tok_Equal ->                                         (* If the next token is '=' *)
    let tok3 = match_token tok2 Tok_Equal in                (* Consume '=' token *)
    let (tok4, expr4) = parse_EqualityExpr tok3 in         (* Parse the right side of the equality expression *)
    (tok4, Binop(Equal, expr2, expr4))                      (* Return the equality expression *)
  |Some Tok_NotEqual ->                                      (* And so on...*)
    let tok3 = match_token tok2 Tok_NotEqual in             
    let (tok4, expr4) = parse_EqualityExpr tok3 in         
    (tok4, Binop(NotEqual, expr2, expr4))                   
  |_ -> (tok2, expr2)                                        


and parse_RelationalExpr toks =
  let (tok2, expr2) = parse_AdditiveExpr toks in            (* Parse the left side of the relational expression *)
  match lookahead tok2 with
  |Some Tok_LessEqual ->                                     
    let tok3 = match_token tok2 Tok_LessEqual in            
    let (tok4, expr4) = parse_RelationalExpr tok3 in       
    (tok4, Binop(LessEqual, expr2, expr4))                  
  |Some Tok_GreaterEqual ->                                  
    let tok3 = match_token tok2 Tok_GreaterEqual in         
    let (tok4, expr4) = parse_RelationalExpr tok3 in       
    (tok4, Binop(GreaterEqual, expr2, expr4))               
  |Some Tok_Less ->                                          
    let tok3 = match_token tok2 Tok_Less in                
    let (tok4, expr4) = parse_RelationalExpr tok3 in       
    (tok4, Binop(Less, expr2, expr4))                       
  |Some Tok_Greater ->                                       
    let tok3 = match_token tok2 Tok_Greater in             
    let (tok4, expr4) = parse_RelationalExpr tok3 in       
    (tok4, Binop(Greater, expr2, expr4))                    
  |_ -> (tok2, expr2)                                        


and parse_AdditiveExpr toks =
  let (tok2, expr2) = parse_MultiplicativeExpr toks in      (* Parse the left side of the additive expression *)
  match lookahead tok2 with
  |Some Tok_Add ->                                           
    let tok3 = match_token tok2 Tok_Add in                 
    let (tok4, expr4) = parse_AdditiveExpr tok3 in         
    (tok4, Binop(Add, expr2, expr4))                        
  |Some Tok_Sub ->                                           
    let tok3 = match_token tok2 Tok_Sub in                 
    let (tok4, expr4) = parse_AdditiveExpr tok3 in         
    (tok4, Binop(Sub, expr2, expr4))                        
  |_ -> (tok2, expr2)                                        


and parse_MultiplicativeExpr toks =
  let (tok2, expr2) = parse_ConcatExpr toks in             (* Parse the left side of the multiplicative expression *)
  match lookahead tok2 with
  |Some Tok_Mult ->                                          
    let tok3 = match_token tok2 Tok_Mult in                
    let (tok4, expr4) = parse_MultiplicativeExpr tok3 in   
    (tok4, Binop(Mult, expr2, expr4))                       
  |Some Tok_Div ->                                           
    let tok3 = match_token tok2 Tok_Div in                 
    let (tok4, expr4) = parse_MultiplicativeExpr tok3 in   
    (tok4, Binop(Div, expr2, expr4))                        
  |_ -> (tok2, expr2)                                        


and parse_ConcatExpr toks =
  let (tok2, expr2) = parse_UnaryExpr toks in              (* Parse the left side of the concatenation expression *)
  match lookahead tok2 with
  |Some Tok_Concat ->                                        
    let tok3 = match_token tok2 Tok_Concat in              
    let (tok4, expr4) = parse_ConcatExpr tok3 in          
    (tok4, Binop(Concat, expr2, expr4))                     
  |_ -> (tok2, expr2)                                        


and parse_UnaryExpr toks =
  match lookahead toks with
  |Some Tok_Not ->                                            (* If the next token is 'not' *)
    let tok2 = match_token toks Tok_Not in                  
    let (tok3, expr3) = parse_UnaryExpr tok2 in            
    (tok3, Not(expr3))                                      
  |_ -> parse_FunctionCallExpr toks                         


and parse_FunctionCallExpr toks =
  let (tok2, expr2) = parse_PrimaryExpr toks in            (* Parse primary expression *)
  match lookahead tok2 with
  |Some (Tok_Int _) ->                                       (* If the next token is an integer *)
    let (tok3, expr3) = parse_PrimaryExpr tok2 in          (* Parse primary expression *)
    (tok3, App(expr2, expr3))                               (* Return function call expression *)
  |Some (Tok_Bool _) ->                                      (* And so on... *)
    let (tok3, expr3) = parse_PrimaryExpr tok2 in          
    (tok3, App(expr2, expr3))                               
  |Some (Tok_String _) ->                                    
    let (tok3, expr3) = parse_PrimaryExpr tok2 in          
    (tok3, App(expr2, expr3))                               
  |Some (Tok_ID _) ->                                        
    let (tok3, expr3) = parse_PrimaryExpr tok2 in          
    (tok3, App(expr2, expr3))                               
  |Some Tok_LParen ->                                         
    let (tok3, expr3) = parse_PrimaryExpr tok2 in          
    (tok3, App(expr2, expr3))                               
  |_ -> (tok2, expr2)                                        (* If not, return primary expression *)

(*
and parse_PrimaryExpr toks =
  match lookahead toks with
  |Some (Tok_Int i) -> let tok2 = match_token toks (Tok_Int i) in (tok2, Value(Int i))   
  |Some (Tok_Bool b) -> let tok2 = match_token toks (Tok_Bool b) in (tok2, Value(Bool b))  
  |Some (Tok_String s) -> let tok2 = match_token toks (Tok_String s) in (tok2, Value(String s))  
  |Some (Tok_ID d) -> let tok2 = match_token toks (Tok_ID d) in (tok2, ID d)               
  |Some Tok_LParen -> let tok2 = match_token toks Tok_LParen in                          (* If the next token is a left parenthesis *)
    let (tok3, expr3) = parse_Expr tok2 in                                               (* Parse the expression within the parentheses *)
    let tok4 = match_token tok3 Tok_RParen in                                            (* Consume the right parenthesis *)
    (tok4, expr3)                                                                         (* Return the expression *)
  |_ -> raise (InvalidInputException ":(")      
*)
and parse_PrimaryExpr toks = 
  match (lookahead toks) with
  | Some (Tok_Int a) -> let toks2 = match_token toks (Tok_Int a) in
    (toks2, Int a)
  | Some (Tok_Bool b) -> let toks2 = match_token  toks (Tok_Bool b) in
    (toks2, Bool b)
  | Some (Tok_String c) -> let toks2 = match_token  toks (Tok_String c) in
    (toks2, String c)
  | Some (Tok_ID d) -> let toks2 = match_token toks (Tok_ID d) in
    (toks2, ID d)
  | Some Tok_LParen -> let toks2 = match_token toks Tok_LParen in
    let (toks3, expr_after_Expr) = parse_Expr toks2 in
    let toks4 = match_token  toks3 Tok_RParen in
    (toks4, expr_after_Expr)
  | Some Tok_LCurly -> parse_RecordExpr toks
  | _ -> raise (InvalidInputException "Invalid primary expression")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  let (rem_toks, expr) = parse_Mutop toks in
  if rem_toks <> [] then raise (InvalidInputException ":(")
  else (rem_toks, expr)

and parse_Mutop toks =             (*parsing individual mutops based on the lookahead token*)
  match lookahead toks with
  |Some Tok_Def -> parse_DefMutop toks
  |Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
  |Some Tok_Let -> parse_ExprMutop toks
  |Some Tok_Fun -> parse_ExprMutop toks
  |Some Tok_If -> parse_ExprMutop toks
  |Some Tok_Not -> parse_ExprMutop toks
  |Some (Tok_Int _) -> parse_ExprMutop toks
  |Some (Tok_Bool _) -> parse_ExprMutop toks
  |Some (Tok_String _) -> parse_ExprMutop toks
  |Some (Tok_ID _) -> parse_ExprMutop toks
  |Some Tok_LParen -> parse_ExprMutop toks
  |_ -> raise (InvalidInputException ":(")

and parse_DefMutop toks =          (*parses definition mutops, starting with the 'def' keyword*)
  let tok2 = match_token toks Tok_Def in
  match lookahead tok2 with
  |Some (Tok_ID d) -> let tok3 = match_token tok2 (Tok_ID d) in let (tok4, expr4) = parse_Expr (match_token tok3 Tok_Equal) in
    (match_token tok4 Tok_DoubleSemi, Def(d, expr4))
  |_ -> raise (InvalidInputException ":(")

and parse_ExprMutop toks =                       (*parses expression mutops, which can be let, fun, if, not, integers,  *)
  let (tok2, expr2) = parse_Expr toks in         (* booleans, strings,identifiers, or expressions enclosed in parentheses*)
  (match_token tok2 Tok_DoubleSemi, Expr(expr2))
;;

