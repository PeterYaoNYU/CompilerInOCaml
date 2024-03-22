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

let rec make_seq (s:Cish_ast.stmt list): Cish_ast.stmt = 
  match s with
  | s1::[] -> s1
  | s1::res -> (Seq(s1,(make_seq res)),0)
  | [] -> raise UnboundError

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
  (* compile_aux stands for compile auxiliary *)
  let rec compile_aux (e:Scish_ast.exp) env : Cish_ast.exp = 
    match e with
    | Int i -> Int i, 0
    | PrimApp (op, exps) ->
      let compiled_exps = List.map (fun exp -> compile_aux exp env) exps in
      let binop = compile_binop op in
      (match compiled_exps with
      | [e1; e2] -> (Binop(e1, binop, e2), 0), env
      | _ -> failwith "Unsupported number of arguments for binop")
    | Var x -> 
      let depth = lookup env x in 
      (depth_to_exp depth, 0)
    | Lambda (arg, body) ->
      (* insert arg into the environment *)
      let env = insert_arg_with_depth env arg in
      let func_name = fresh_name () in
      let body_exp, new_env = compile_aux body env in
      (* Create the Cish function representing the lambda *)
      let fn = {name = func_name; args = ["dyenv"]; body = (Return body_exp, 0); pos = 0} in
      add_function (Fn fn);
      (* Allocate memory for the closure and assign it to a fresh variable *)
      (* let malloc_var = fresh_name () in *)
      let malloc_exp = (Assign ((Var "result", 0), Malloc (Int 8, 0)), 0) in

      (* Store the function pointer in the first 4 bytes of the allocated space *)
      let func_ptr_store = ((Store (Var "result", 0), (Var func_name, 0)), 0) in

      (* Store the environment pointer in the second 4 bytes of the allocated space *)
      let env_ptr_store = ((Store (Binop((Var "result", 0), Plus, (Int 4, 0)), 0), (Var "dyenv", 0)), 0) in

      (* Sequence the operations: malloc, store the function pointer, store the environment pointer *)
      let seq_ops =  make_seq([malloc_exp; func_ptr_store; env_ptr_store]) in
      seq_ops

      (* let closure = ClosureV {env = malloc_var; body = (Var arg, body)} in
      (* Question: do we need to extend the current env with the new function pointer? *)
      (Var func_name, 0), (!current_env @ [(func_name, closure)]) *)
    | App (e1, e2) -> 
      (* initialize 3 fresh variables for holding different values *)
      let t1 = new_var () in
      let t2 = new_var () in
      let malloc_var = new_var () in
      let init_t1 = Let(t1, (Int 0, 0)) in
      let init_t2 = Let(t2, (Int 0, 0)) in
      let init_malloc_var = Let(malloc_var, (Int 0, 0)) in
      
      (* e1 must be a lambda expression, so *(result) has the function pointer *)
      (* While *(result + 4) has the dyenv *)
      let (closure_name, lambda_env)  = compile_aux e1 env in
      let assign_1 = Assign(Var t1, (Load ((Var "result", 0), 0))), 0 in
      let assign_t2 = Assign(Var t2, (Binop ((Var "result", 0), Plus, (Int 4, 0)), 0)), 0 in 

      (* compile e2, which must be an Int, which is in the variable "result" *)
      let arg_exp, arg_env = compile_aux e2 env in

      (* malloc a new region, the first word is for storing the argument *)
      (* the second word is for storing historic environment, dyenv from e1, which is t2 basically *)
      let malloc_exp = Assign (Var malloc_var, (Malloc (Int 8, 0), 0)), 0 in

      (* look for the closure with the function name *)
      (* let closure = lookup lambda_env closure_name in *)

      (* store the argument in the malloced area *)
      let store_arg = Store ((Var malloc_var, 0), (Var "result", 0)), 0 in

      (* store the historic env in the second word of the malloc region *)
      let store_env = Store ((Binop ((Var malloc_var, 0), Plus, (Int 4, 0)), 0), (Var t2, 0)), 0 in
      let call_exp = Assing ("result" ,(Call ((Var t1, 0), [(Var malloc_var, 0)]), 0)), 0 in

      (* call the function pointer with the environment and the argument *)
      let all_exp = make_seq([init_t1; init_t2; init_malloc_var; assign_1; assign_t2; malloc_exp; store_arg; store_env; call_exp]) in
      all_exp
    | _ -> failwith "Expression type not supported"
  
  in
  let main_body = compile_aux e in
  let main_func = {name = "main"; args = []; body = (Return main_body, 0); pos = 0} in
  add_function (Fn main_func);
  List.rev !global_functions  (* Return the final program *)

