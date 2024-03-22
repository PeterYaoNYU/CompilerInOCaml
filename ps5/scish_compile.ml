open Scish_ast
open Cish_ast

type depth = int

type value = 
  | IntV of int
  | ClosureV of {env: env, body: Var * exp}
(* the env should be var * depth(int) *)
let env = (var * value) list

let depth_to_exp depth : exp =
  let rec build_exp depth current_exp = 
    match depth with 
    | 1 -> Load (current_exp, 0), 0 
    | _ -> 
      let add_exp = Binop(current_exp, Plus, Int 4), 0 in 
      let deref_exp = Load (current_exp, 0), 0 in
      build_exp (depth - 1) deref_exp
  in
  build_exp depth (Var "env")

let increment_env_depth env =
  List.map (fun (var, value) ->
    match value with
    | IntV depth -> (var, IntV (depth + 1))
    | closure -> (var, closure)
  ) env

let insert_arg_with_depth env arg = 
  (arg, IntV 1) :: (increment_env_depth env)

(* A ref to dynamically store Cish functions including lambdas *)
let global_functions = ref []

(* A simplified representation of an environment as a list of strings for demonstration *)
let current_env : env ref = ref [("env", IntV 0)]

(* Unified unique name generator *)
let fresh_name =
  let counter = ref 0 in
  fun () -> incr counter; "t" ^ string_of_int !counter

(* Function to reset the global function list - useful for multiple compilations *)
let reset_global_functions () =
  global_functions := []

(* Helper function to add a new function to the global list *)
let add_function fn =
  global_functions := !global_functions @ [fn]

(* Compile Scish binary operations to Cish operations *)
let compile_binop = function
  | Plus -> Plus
  | Minus -> Minus
  | _ -> failwith "Operation not supported"

(* need change, look up depth instead   *)
let rec lookup env x = 
  match env with
  | ((y, v)::rest) -> if x = y then v else lookup rest x
  | [] -> failwith "unbound variable"

(* The main compilation function *)
let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program =
  reset_global_functions ();  (* Reset for a fresh compilation *)
  let rec compile_aux (e:Scish_ast.exp) env : Cish_ast.exp * env = 
    match e with
    | Int i -> Int i, 0
    | PrimApp (op, exps) ->
      let compiled_exps_envs = List.map (fun exp -> compile_aux exp env) exps in
      let compiled_exps = List.map fst compiled_exps_envs in
      let binop = compile_binop op in
      (match compiled_exps with
      | [e1; e2] -> (Binop(e1, binop, e2), 0), env
      | _ -> failwith "Unsupported number of arguments for binop")
    | Var x -> 
      let depth = lookup env x in 
      (depth_to_exp depth, 0), env
    | Lambda (arg, body) ->
      let func_name = fresh_name () in
      let body_exp, new_env = compile_aux body env in
      (* Create the Cish function representing the lambda *)
      let fn = {name = func_name; args = ["env"]; body = (Return body_exp, 0); pos = 0} in
      add_function (Fn fn);
      (* Allocate memory for the closure and assign it to a fresh variable *)
      let malloc_var = fresh_name () in
      let malloc_exp = (Assign (Var malloc_var, Malloc (Int 8, 0)), 0) in

      (* Store the function pointer in the first 4 bytes of the allocated space *)
      let func_ptr_store = (Store ((Load (Var malloc_var, 0), 0), Var func_name), 0) in

      (* Store the environment pointer in the second 4 bytes of the allocated space *)
      let env_ptr_store = (Store (Load (Binop (Var malloc_var, Plus, Int 4, 0), 0), new_env), 0) in

      (* Sequence the operations: malloc, store the function pointer, store the environment pointer *)
      let seq_ops = Seq (malloc_exp, func_ptr_store, 0) in

      let closure = ClosureV {env = malloc_var; body = (Var arg, body)} in
      (* Question: do we need to extend the current env with the new function pointer? *)
      (Var func_name, 0), (!current_env @ [(func_name, closure)])
    | App (e1, e2) -> 
      let (closure_name, lambda_env)  = compile_aux e1 env in
      let arg_exp, arg_env = compile_aux e2 env in
      (* malloc a new region, the first word is for storing the argument *)
      (* the second word is for storing historic environment *)
      let malloc_var = fresh_name () in
      let malloc_exp = Assign (Var malloc_var, Malloc (Int 8, 0)), 0 in

      (* look for the closure with the function name *)
      let closure = lookup lambda_env closure_name in

      (* adjust the environment, add the lambda argument into the env*)
      let adjusted_env = insert_arg_with_depth lambda_env (fst closure.body)  in
      
      (* load the function pointer from the malloced area by lambda *)
      let func_ptr = fresh_name () in
      let load_func_ptr = Assign(Var func_ptr, Load (Var closure.env, 0)), 0  in

      (* load the environment caputured, stored in env_ptr *)
      let env_ptr = fresh_name () in
      let load_env_ptr = Assign(Var env_ptr, Load (Binop (Var closure.env, Plus, Int 4, 0), 0), 0) in

      (* store the argument in the malloced area *)
      let store_arg = Store (Var malloc_var, Int arg_exp), 0 in

      (* store the historic env in the second word of the malloc region *)
      let store_env = Store (Binop (Var closure.env, Plus, Int 4, 0), Var env_ptr), 0 in
      let call_exp = Call (Var func_ptr, [Var malloc_var]), 0 in

      (* call the function pointer with the environment and the argument *)
      let all_exp = Seq (malloc_exp, Seq (load_func_ptr, Seq (load_env_ptr, Seq (store_arg, Seq (store_env, call_exp, 0), 0), 0), 0), 0) in
      all_exp, adjusted_env
    | _ -> failwith "Expression type not supported"
  
  in
  let main_body = compile_aux e in
  let main_func = {name = "main"; args = []; body = (Return main_body, 0); pos = 0} in
  add_function (Fn main_func);
  List.rev !global_functions  (* Return the final program *)

