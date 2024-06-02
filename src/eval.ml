open Types


(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)

let rec eval_expr env e = 
  let rec eval_binop op e1 e2 =
    match (eval_expr env e1, eval_expr env e2) with
    | Int n1, Int n2 -> (
        match op with
        | Add -> Int (n1 + n2)
        | Sub -> Int (n1 - n2)
        | Mult -> Int (n1 * n2)
        | Div -> if n2 = 0 then raise DivByZeroError else Int (n1 / n2)
        | Greater -> Bool (n1 > n2)
        | Less -> Bool (n1 < n2)
        | GreaterEqual -> Bool (n1 >= n2)
        | LessEqual -> Bool (n1 <= n2)
        | Equal -> Bool (n1 = n2)
        | NotEqual -> Bool (n1 <> n2)
        | _ -> raise (TypeError "Expected type bool") )
    | Bool b1, Bool b2 -> (
        match op with
        | And -> Bool (b1 && b2)
        | Or -> Bool (b1 || b2)
        | Equal -> Bool (b1 = b2)
        | NotEqual -> Bool (b1 <> b2)
        | _ -> raise (TypeError "Invalid operation for booleans") )
    | String s1, String s2 -> (
        match op with
        | Concat -> String (s1 ^ s2)
        | Equal -> Bool (s1 = s2)
        | NotEqual -> Bool (s1 <> s2)
        | _ -> raise (TypeError "Invalid operation for strings") )
    | _ -> raise (TypeError "Cannot compare types")
  in
  match e with
  | Int _ | Bool _ | String _ -> e
  | ID x -> (
      try !(List.assoc x env) with Not_found -> raise (DeclareError ("Unbound variable " ^ x))
    )
  | Fun (x, body) -> Closure (env, x, body)
  | Not e' -> (
      match eval_expr env e' with
      | Bool b -> Bool (not b)
      | _ -> raise (TypeError "Expected type bool for not operation") )
  | Binop (op, e1, e2) -> eval_binop op e1 e2
  | If (guard, e1, e2) -> (
      match eval_expr env guard with
      | Bool true -> eval_expr env e1
      | Bool false -> eval_expr env e2
      | _ -> raise (TypeError "Guard expression must be of type bool") )
  | Let (x,false,init,body) -> eval_expr (extend env x (eval_expr env init)) body 
  | Let (x,true,init,body) ->
      let newEnv = extend_tmp env x in
      update newEnv x (eval_expr (newEnv) init);
      eval_expr newEnv body
  | App (e1, e2) -> (
      match eval_expr env e1 with
      | Closure (en, x, body) ->
          let newEnv  = extend en x (eval_expr env e2) in 
           eval_expr newEnv body
      | _ -> raise (TypeError "Not a function") )
  | Record fields ->
      Record (List.map (fun (label, e') -> (label, eval_expr env e')) fields)
  | Select (label, record_expr) -> (
      match eval_expr env record_expr with
      | Record fields -> (
          try List.assoc label fields with
          | Not_found -> raise (SelectError "Label not found") )
      | _ -> raise (TypeError "Not a record") )
  | Closure _ -> e (* For lexical scoping *)

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)

let eval_mutop env m = 
  match m with 
  | NoOp -> (env, None)  
  | Expr e -> (env, Some (eval_expr env e))
  | Def (x, e) -> let envNew = extend_tmp env x in
      let value = (eval_expr envNew e) in
      update envNew x value;
      (envNew, Some value)