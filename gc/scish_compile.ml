open Scish_ast
open Cish_ast

exception UnboundError

type depth = int

type value = 
  | IntV of int
  (* | ClosureV of {env: env, body: Var * exp} *)
(* the env should be var * depth(int) *)
type env = (string * value) list

let depth_to_exp depth : Cish_ast.exp =
  let rec build_exp depth current_exp : Cish_ast.exp = 
    match depth with 
    | 1 -> Load (current_exp), 0 
    | _ -> 
      let add_exp = (Binop(current_exp, Plus, (Int 4, 0)), 0) in 
      let deref_exp = (Load add_exp, 0) in
      build_exp (depth - 1) deref_exp
  in
  build_exp depth (Var "dyenv", 0)


let rec make_seq (s:Cish_ast.stmt list): Cish_ast.stmt = 
  match s with
  | s1::[] -> s1
  | s1::res -> (Seq(s1,(make_seq res)),0)
  | [] -> raise UnboundError

let increment_env_depth env =
  List.map (fun (var, value) ->
    match value with
    | IntV depth -> (var, IntV (depth + 1))
    (* | closure -> (var, closure) *)
  ) env

let insert_arg_with_depth env arg = 
  (arg, IntV 1) :: increment_env_depth env
  

(* A ref to dynamically store Cish functions including lambdas *)
let global_functions = ref []

(* A simplified representation of an environment as a list of strings for demonstration *)
let current_env : env ref = ref []

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
  | Scish_ast.Plus -> Cish_ast.Plus
  | Scish_ast.Minus -> Cish_ast.Minus
  | Scish_ast.Times -> Cish_ast.Times
  | Scish_ast.Div -> Cish_ast.Div
  | Scish_ast.Eq -> Cish_ast.Eq
  | Scish_ast.Lt -> Cish_ast.Lt
  | _ -> failwith "Operation not supported"

(* need change, look up depth instead   *)
let rec lookup env x = 
  match env with
  | ((y, IntV depth)::rest) -> if x = y then depth else lookup rest x
  | [] -> failwith "unbound variables"

(* The main compilation function *)
let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program =
  reset_global_functions ();  (* Reset for a fresh compilation *)
  (* compile_aux stands for compile auxiliary *)
  let rec compile_aux (e:Scish_ast.exp) (env:(Scish_ast.var * value) list) : Cish_ast.stmt = 
    match e with
    | Int i -> Exp(Assign ("result", (Int i, 0)), 0), 0
    | PrimApp (op, exps) ->
      (
        match op with 
        | Scish_ast.Plus | Scish_ast.Minus | Scish_ast.Times | Scish_ast.Div | Scish_ast.Eq | Scish_ast.Lt -> 
          let compile_first = compile_aux (List.hd exps) env in
          let temp = fresh_name() in  
          let assign_temp = Exp(Assign(temp,(Var "result",0)),0),0 in
          let compile_second = compile_aux (List.hd (List.tl exps)) env in
          let binop = compile_binop op in
          let compute_stmt = Exp(Assign("result",(Binop((Var temp,0),binop,(Var "result",0)),0)),0),0 in
          let return_stmt = make_seq([compile_first; assign_temp; compile_second; compute_stmt]) in
          Let(temp,(Int 0,0),return_stmt),0
        | Scish_ast.Cons ->
          let t = fresh_name() in 
          let malloc_exp = Exp(Assign(t, (Malloc (Int 8, 0), 0)), 0), 0 in
          let compile_first = compile_aux (List.hd exps) env in
          let store_first = Exp(Store((Var t,0),(Var "result",0)),0),0 in
          let compile_second = compile_aux (List.hd (List.tl exps)) env in
          let store_second = Exp(Store((Binop((Var t,0),Plus,(Int 4,0)),0),(Var "result",0)),0),0 in
          let assign_result = Exp(Assign("result", (Var t, 0)), 0), 0 in
          Let (t, (Int 0, 0), make_seq([malloc_exp; compile_first; store_first; compile_second; store_second; assign_result])), 0
        | Scish_ast.Fst -> 
          let compile_first = compile_aux (List.hd exps) env in
          let assign_result = Exp(Assign("result", (Load((Var "result",0)),0)),0),0 in
          make_seq([compile_first; assign_result])
        | Scish_ast.Snd ->
          let compile_first = compile_aux (List.hd exps) env in
          let assign_result = Exp(Assign("result", (Load((Binop((Var "result",0),Plus,(Int 4,0)),0)),0)),0),0 in
          make_seq([compile_first; assign_result])
        | _ -> failwith "Operation not supported"
      )
    | Var x -> 
      let depth = lookup env x in 
      let derefernece_stmt = (depth_to_exp depth) in
      let assign_stmt = Exp(Assign("result", derefernece_stmt), 0), 0 in
      assign_stmt
    | Lambda (arg, body) ->
      (* insert arg into the environment *)
      let env = insert_arg_with_depth env arg in
      let func_name = fresh_name () in
      let body_exp= compile_aux body env in
      (* Create the Cish function representing the lambda *)
      let fn = {name = func_name; args = ["dyenv"]; body =
        (Let("result",((Int 0),0), 
          (Seq((body_exp),(Return((Var "result"),0),0)),0)),0)
        ; pos = 0} in
      add_function (Fn fn);
      (* Allocate memory for the closure and assign it to a fresh variable *)
      (* let malloc_var = fresh_name () in *)
      let malloc_exp = Assign ("result", (Malloc (Int 8, 0), 0)), 0 in

      (* Store the function pointer in the first 4 bytes of the allocated space *)
      let func_ptr_store = Store ((Var "result", 0), (Var func_name, 0)), 0 in

      (* Store the environment pointer in the second 4 bytes of the allocated space *)
      let env_ptr_store = (Store ((Binop((Var "result", 0), Plus, (Int 4, 0)), 0), (Var "dyenv", 0)), 0) in

      (* Sequence the operations: malloc, store the function pointer, store the environment pointer *)
      let seq_ops =  make_seq([ Exp malloc_exp, 0; Exp func_ptr_store, 0; Exp env_ptr_store, 0]) in
      seq_ops

      (* let closure = ClosureV {env = malloc_var; body = (Var arg, body)} in
      (* Question: do we need to extend the current env with the new function pointer? *)
      (Var func_name, 0), (!current_env @ [(func_name, closure)]) *)
    | App (e1, e2) -> 
      (* initialize 3 fresh variables for holding different values *)
      let t1 = fresh_name () in
      let t2 = fresh_name () in
      let malloc_var = fresh_name () in
      (* let init_t1 = Let(t1,(Int 0,0)), 0 in
      let init_t2 = Let(t2,(Int 0,0)), 0 in
      let init_malloc_var = Let(malloc_var,(Int 0,0)), 0 in *)
      
      (* e1 must be a lambda expression, so *(result) has the function pointer *)
      (* While *(result + 4) has the dyenv *)
      let lambda_stmt  = compile_aux e1 env in
      let assign_1 = Exp(Assign(t1, (Load (Var "result", 0), 0)), 0), 0 in
      let assign_t2 =Exp(Assign(t2, (Load(Binop((Var "result", 0), Plus, (Int 4, 0)), 0), 0)), 0), 0
      in
      (* compile e2, which must be an Int, which is in the variable "result" *)
      let arg_exp= compile_aux e2 env in
      (* let arg_stmt = Exp(Assign("result", arg_exp), 0), 0 in *)
      (* malloc a new region, the first word is for storing the argument *)
      (* the second word is for storing historic environment, dyenv from e1, which is t2 basically *)
      let malloc_exp = Exp(Assign (malloc_var, (Malloc (Int 8, 0), 0)), 0), 0 in

      (* look for the closure with the function name *)
      (* let closure = lookup lambda_env closure_name in *)

      (* store the argument in the malloced area *)
      let store_arg = Exp(Store ((Var malloc_var, 0), (Var "result", 0)), 0), 0 in

      (* store the historic env in the second word of the malloc region *)
      let store_env = Exp(Store ((Binop ((Var malloc_var, 0), Plus, (Int 4, 0)), 0), (Var t2, 0)), 0), 0 in
      let call_exp = Exp(Assign ("result" ,(Call ((Var t1, 0), [(Var malloc_var, 0)]), 0)), 0), 0 in

      (* call the function pointer with the environment and the argument *)
      let all_exp = make_seq([lambda_stmt; assign_1; assign_t2; arg_exp; malloc_exp; store_arg; store_env; call_exp]) in
      (
        Let(t1,(Int 0,0),(
          Let(t2,(Int 0,0),(
            Let(malloc_var,(Int 0,0),
            all_exp)
          ,0))
        ,0))
    ,0)
    | If (e1, e2, e3) -> 
      let compile_e1 = compile_aux e1 env in
      let t = fresh_name () in
      let assign_t = Exp(Assign(t, (Var "result", 0)), 0), 0 in
      let compile_e2 = compile_aux e2 env in
      let compile_e3 = compile_aux e3 env in
      let if_stmt = If((Var t, 0), compile_e2, compile_e3), 0 in
      let all_stmt = make_seq([compile_e1; assign_t; if_stmt]) in 
      Let(t, (Int 0, 0), all_stmt), 0
    | _ -> failwith "Expression type not supported"
  
  in
  (* let main_body = compile_aux e in *)
  let main_func = {name = "main"; args = []; body = 
    (Let("dyenv",(Int 0,0),
      (Let("result",(Int 0,0), 
      (Seq((compile_aux e []),
      (Return(Var "result",0),0)),0)
    ),0)),0)
    ; pos = 0} in
  add_function (Fn main_func);
  List.rev !global_functions  (* Return the final program *)

