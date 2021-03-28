open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.iter (fun (var, value) ->
      let vs = match value with
        | Int_Val(i) -> string_of_int i
        | Bool_Val(b) -> string_of_bool b in
      Printf.printf "- %s => %s\n" var vs) (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      let vs = match value with
        | Int_Val(i) -> string_of_int i
        | Bool_Val(b) -> string_of_bool b in
      acc ^ (Printf.sprintf "- %s => %s\n" var vs)) "" (prune_env env)



(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
  match e with 
  | Var(x) -> 
    if List.mem_assoc x env then
      List.assoc x env
    else
      raise (UndefinedVar)
  | Number(x) ->
    Int_Val(x)
  | Plus(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Int_Val(n1), Int_Val(n2) -> Int_Val(n1 + n2)
    | _, _ -> raise (TypeError))
  | Minus(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Int_Val(n1), Int_Val(n2) -> Int_Val(n1 - n2)
    | _, _ -> raise (TypeError))
  | Times(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Int_Val(n1), Int_Val(n2) -> Int_Val(n1 * n2)
    | _, _ -> raise (TypeError))
  | Div(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Int_Val(n1), Int_Val(n2) ->
      if n2 <> 0 then
        Int_Val(n1 / n2)
      else
        raise (DivByZeroError)
    | _, _ -> raise (TypeError))
  | Mod(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Int_Val(n1), Int_Val(n2) ->
      if n2 <> 0 then
        Int_Val(n1 mod n2)
      else
        raise (DivByZeroError)
    | _, _ -> raise (TypeError))
  | Eq(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Int_Val(n1), Int_Val(n2) -> Bool_Val(n1 = n2)
    | Bool_Val(b1), Bool_Val(b2) -> Bool_Val(b1 = b2)
    | _, _ -> raise (TypeError))
  | Leq(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Int_Val(n1), Int_Val(n2) -> Bool_Val(n1 <= n2)
    | _, _ -> raise (TypeError))
  | Lt(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Int_Val(n1), Int_Val(n2) -> Bool_Val(n1 < n2)
    | _, _ -> raise (TypeError))
  | Not(e) ->
    let v = eval_expr e env in
    (match v with
    | Bool_Val(b) -> Bool_Val(not b)
    | _ -> raise (TypeError))
  | And(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Bool_Val(b1), Bool_Val(b2) -> Bool_Val(b1 && b2)
    | _, _ -> raise (TypeError))
  | Or(e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    (match v1, v2 with
    | Bool_Val(b1), Bool_Val(b2) -> Bool_Val(b1 || b2)
    | _, _ -> raise (TypeError))
  | True ->
    Bool_Val(true)
  | False ->
    Bool_Val(false)

let rec do_n (n : int) (c : com) (env : environment) (f : com  -> environment -> environment) : environment =
  match n with
  | 0 -> env
  | m ->
    let new_env = f c env in
    do_n (m-1) (c) (new_env) (f)

(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
  match c with
  | While(cond, body) ->
    (match (eval_expr cond env) with
    | Bool_Val(true) -> 
      let new_env = eval_command body env in
      eval_command c new_env
    | Bool_Val(false) -> env
    | _ -> raise (TypeError))
  | For(e, body) ->
    (match (eval_expr e env) with
    | Int_Val(n) -> 
      if n = 0 then
        env
      else
        do_n n body env eval_command
    | _ -> raise (TypeError))
  | Cond(e, c1, c2) ->
    (match (eval_expr e env) with
    | Bool_Val(true) -> 
      eval_command c1 env
    | Bool_Val(false) ->
      eval_command c2 env
    | _ -> raise (TypeError))
  | Comp(c1, c2) ->
    let new_env = eval_command c1 env in
    eval_command c2 new_env
  | Assg(s, e) ->
    if List.mem_assoc s env then
      let v = eval_expr e env in
      let current_v = List.assoc s env in
      (match v, current_v with
      | Int_Val(n1), Int_Val(n2) -> (s, v) :: env
      | Bool_Val(b1), Bool_Val(b2) -> (s, v) :: env
      | _, _ -> raise (TypeError))
    else
      raise (UndefinedVar)
  | Declare(d, s) ->
    let new_value = 
      match d with
      | Int_Type -> Int_Val(0)
      | Bool_Type -> Bool_Val(false) in
    (s, new_value) :: env
  | Skip ->
    env
