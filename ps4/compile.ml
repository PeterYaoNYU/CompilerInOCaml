(* Compile Cish AST to MIPS AST *)
open Mips

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* keep an env for each procedure in the compiler, BEGIN Here *)

(* Environment to keep track of variable offsets and labels *)
type environment = {
  varmap : (string * int) list;  (* Maps variable names to their offsets on the stack *)
  stack_offset : int;            (* Current offset from the frame pointer *)
  (* label_end : string;            Label for the function end (for the epilogue) *)
}


(* stack offset is just a naming convetion that I adopted *)
(* Ideally it should be called a frame offset, because it is relative to the frame pointer *)
(* Create a new, empty environment *)
let new_env () = {
  varmap = [];
  stack_offset = 0;
  (* label_end = new_label (); *)
}

(* Push a register onto the stack *)
let push reg env = 
  let code = [Mips.Add(Mips.R29, Mips.R29, Immed (Word32.neg (Word32.fromInt (4))));  (* Decrement the stack pointer *)
              Mips.Sw(reg, Mips.R29, Word32.fromInt 0)] in     (* Store the register on the stack *)
  (* somehow I don't feel like updating the stack offset here, it feels redundant, but I am not sure *)
  env, code

(* Pop a register from the stack *)
let pop reg env = 
  let code = [Mips.Lw(reg, Mips.R29, Word32.fromInt 0);        (* Load the value from the stack into the register *)
              Mips.Add(Mips.R29, Mips.R29,Immed (Word32.fromInt 4))] in  (* Increment the stack pointer *)
  env, code

(* Add a new variable to the environment *)
let add_var v env = 
  let offset = env.stack_offset - 4 in
  { env with varmap = (v, offset) :: env.varmap; stack_offset = offset }

(* Look up the stack offset of a variable
let rec lookup_var env v = 
  match env.varmap with
  | (var, offset) :: _ when var = v -> offset
  | _ :: rest -> lookup_var { env with varmap = rest } v
  | [] -> raise (Failure ("Variable not found: " ^ v)) *)

(* to handle the case where the assign exp is assigning to a new variable *)
(* here we redefine the lookup_var fn *)
(* Look up the stack offset of a variable, returning None if not found *)
  let rec lookup_var env v = 
    match env.varmap with
    | (var, offset) :: _ when var = v -> Some offset
    | _ :: rest -> lookup_var { env with varmap = rest } v
    | [] -> None


(* keep an env for each procedure in the compiler, END Here *)

let rec compile_exp (exp:Ast.exp) env : Mips.inst list * environment =
  match exp with
  | Int n, _ ->([Mips.Li (Mips.R2, Word32.fromInt n)], env)  (* Load immediate value into R2 *)
  | Var v, _ ->
      (
          match lookup_var env v with
          | None -> raise (Failure ("Variable not found: " ^ v))
          | Some offset -> ([Mips.Lw (Mips.R2, Mips.R30, Word32.fromInt offset)], env)  (* Load the value from the variable into R2 *)
          (* R30 is typically the MIPS frame pointer *)
      )
  | Binop (e1, op, e2), _ -> 
      (
          let e1_code, env1 = compile_exp e1 env in
          let env2, push_code = push Mips.R2 env1 in
          let e2_code, env3 = compile_exp e2 env2 in
          let env4, pop_code = pop Mips.R8 env3 in
          let op_code = match op with
              | Ast.Plus -> [Mips.Add (Mips.R2, Mips.R2, Reg Mips.R8)]  (* Add R8 to R2 and store the result in R2 *)
              | Ast.Minus -> [Mips.Sub (Mips.R2, Mips.R8, Mips.R2)]  (* Subtract R8 from R2 and store the result in R2 *)
              | Ast.Times -> [Mips.Mul (Mips.R2, Mips.R2, Mips.R8)]  (* Multiply R2 by R8 and store the result in R2 *)
              | Ast.Div -> [Mips.Div (Mips.R2, Mips.R8, Mips.R2)]  (* Divide R2 by R8 *)
              | Ast.Lt -> [Mips.Slt (Mips.R2, Mips.R8, Reg Mips.R2)]  (* Set R2 to 1 if R2 is less than R8, otherwise set it to 0 *)
              | Ast.Gt -> [Mips.Slt (Mips.R2, Mips.R2, Reg Mips.R8)]  (* Set R2 to 1 if R8 is less than R2, otherwise set it to 0 *)
              | Ast.Gte -> 
                  [Mips.Slt (Mips.R2, Mips.R8, Reg Mips.R2);  (* Set R2 to 1 if R2 is less than R8, which means R8 >= R2 *)
                  Mips.Xor (Mips.R2, Mips.R2, Immed (Word32.fromInt 1))]  (* Invert the result *)
              | Ast.Lte -> 
                  [Mips.Slt (Mips.R2, Mips.R2, Reg Mips.R8);  (* Set R2 to 1 if R8 is less than R2, which means R2 <= R8 *)
                  Mips.Xor (Mips.R2, Mips.R2, Immed (Word32.fromInt 1))]  (* Invert the result *)
              | Ast.Eq -> 
                  [Mips.Seq (Mips.R2, Mips.R2, Mips.R8)]  (* Set R2 to 1 if R2 is equal to R8 *)
              | Ast.Neq ->
                  [Mips.Sne (Mips.R2, Mips.R2, Mips.R8)]  (* Set R2 to 1 if R2 is not equal to R8 *)
          in
          (e1_code @ push_code @ e2_code @ pop_code @ op_code, env4)
      )
  | Not e, _ -> 
      (
          let label_true = new_label () in
          let label_end = new_label () in
          let e_code, env = compile_exp e env in  (* Compile the expression e *)
          (e_code @ [Mips.Beq (Mips.R2, Mips.R0, label_true)] @  (* Jump to label_true if R2 is 0 *)
          [Mips.Li (Mips.R2, Word32.fromInt 0); Mips.J label_end; Mips.Label label_true] @  (* Load 0 into R2 and jump to label_end *)
          [Mips.Li (Mips.R2, Word32.fromInt 1); Mips.Label label_end], env)  (* Load 1 into R2 *)
      )
  | And (e1, e2), _ -> 
      (
          let label_false = new_label () in
          let label_end = new_label () in
          let e1_code, env = compile_exp e1 env in  (* Compile the expression e1 *)
          let e2_code, env = compile_exp e2 env in  (* Compile the expression e2 *)
          (e1_code @ [Mips.Beq (Mips.R2, Mips.R0, label_false)] @  (* Jump to label_false if R2 is 0 *)
          e2_code @ [Mips.Beq (Mips.R2, Mips.R0, label_false)] @  (* Jump to label_false if R2 is 0 *)
          [Mips.Li (Mips.R2, Word32.fromInt 1); Mips.J label_end; Mips.Label label_false] @  (* Load 1 into R2 and jump to label_end *)
          [Mips.Li (Mips.R2, Word32.fromInt 0); Mips.Label label_end], env)  (* Load 0 into R2 *)
      )
  | Or (e1, e2), _ ->
      (
          let label_true = new_label () in
          let label_end = new_label () in
          let e1_code, env = compile_exp e1 env in  (* Compile the expression e1 *)
          let e2_code, env = compile_exp e2 env in  (* Compile the expression e2 *)
          (e1_code @ [Mips.Bne (Mips.R2, Mips.R0, label_true)] @  (* Jump to label_true if R2 is not 0 *)
          e2_code @ [Mips.Bne (Mips.R2, Mips.R0, label_true)] @  (* Jump to label_true if R2 is not 0 *)
          [Mips.Li (Mips.R2, Word32.fromInt 0); Mips.J label_end; Mips.Label label_true] @  (* Load 0 into R2 and jump to label_end *)
          [Mips.Li (Mips.R2, Word32.fromInt 1); Mips.Label label_end], env) (* Load 1 into R2 *)
      )
  | Assign (v, e), _ -> 
      (
          let e_code, env = compile_exp e env in  (* Compile the expression e *)
          let offset, env = 
            match lookup_var env v with
            | Some off -> off, env  (* Variable exists, use its offset *)
            | None ->
              (* Variable does not exist; allocate space on the stack for it *)
              let new_offset = env.stack_offset - 4 in  (* Assume each variable takes 4 bytes *)
              let new_env = { env with 
                                varmap = (v, new_offset) :: env.varmap; 
                                stack_offset = new_offset } in  (* Update the environment with the new variable and its offset *)
              new_offset, new_env
          in
          let store_code = [Mips.Sw (Mips.R2, Mips.R30, Word32.fromInt offset)] in  (* Store the result of expression e into the stack location of variable v *)
          e_code @ store_code, env  (* Combine the code for the expression and the store operation, and return the updated environment *)
      )
  | Call (f_name, args), _ ->
    (* Save caller-saved registers *)
    (* let saved_regs = [Mips.R8; Mips.R9; Mips.R10; Mips.R11; Mips.R12; Mips.R13; Mips.R14; Mips.R15; Mips.R24; Mips.R25] in *)
    let saved_regs = [Mips.R8] in
    let env, save_caller_saved_code = List.fold_left (fun (env_accum, code_accum) reg ->
      let env', code = push reg env_accum in
      (env', code_accum @ code)
    ) (env, []) saved_regs in


    (* Evaluate each argument and move the result to the correct register or stack. *)
    let rec eval_args args_code args env arg_idx =
      match args with
      | [] -> args_code, env
      | arg :: rest ->
        let arg_code, env' = compile_exp arg env in
        if arg_idx < 4 then
          (* For the first four arguments, use registers $a0 to $a3. *)
          let reg = match arg_idx with
            | 0 -> Mips.R4
            | 1 -> Mips.R5
            | 2 -> Mips.R6
            | 3 -> Mips.R7
            | _ -> raise (Failure "Argument register index out of bounds")
          in
          let move_to_reg = [Mips.Add (reg, Mips.R2, Mips.Reg Mips.R0)] in
          eval_args (args_code @ arg_code @ move_to_reg) rest env' (arg_idx + 1)
        else
          (* For arguments beyond the first four, push them onto the stack. *)
          let push_to_stack = push Mips.R2 env' in
          eval_args (args_code @ arg_code @ snd push_to_stack) rest (fst push_to_stack) (arg_idx + 1)
    in
    let args_code, env_after_args = eval_args [] args env 0 in
    let call_instr = [Mips.Jal f_name] in

    (* Restore caller-saved registers in reverse order *)
    let env, restore_caller_saved_code = List.fold_left (fun (env_accum, code_accum) reg ->
      let env', code = pop reg env_accum in
      (env', code @ code_accum)
    ) (env_after_args, []) (List.rev saved_regs) in
    (* TODO: also pop the arguments off the stack *)
    (* Combine argument evaluation code, the function call, and potentially handle the return value. *)
    save_caller_saved_code @ args_code @ call_instr @ restore_caller_saved_code, env
  


(* Reusing the code from compiling the fish program *)
(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
(* Compile a Cish statement to MIPS assembly, along with updated environment *)
let rec compile_stmt ((s, _): Ast.stmt) env : Mips.inst list * environment = 
  match s with
  | Ast.Exp exp ->
      let exp_code, updated_env = compile_exp exp env in
      (exp_code, updated_env)
  
  | Ast.Seq (stmt1, stmt2) ->
      let stmt1_code, env_after_stmt1 = compile_stmt stmt1 env in
      let stmt2_code, env_after_stmt2 = compile_stmt stmt2 env_after_stmt1 in
      (stmt1_code @ stmt2_code, env_after_stmt2)
  
  | Ast.If (exp, stmt1, stmt2) ->
      let exp_code, env_after_exp = compile_exp exp env in
      let else_label = new_label () in
      let end_label = new_label () in
      let stmt1_code, env_after_stmt1 = compile_stmt stmt1 env_after_exp in
      let stmt2_code, env_after_stmt2 = compile_stmt stmt2 env_after_exp in
      (exp_code @ [Mips.Beq (Mips.R2, Mips.R0, else_label)] @
       stmt1_code @ [Mips.J end_label; Mips.Label else_label] @
       stmt2_code @ [Mips.Label end_label], env_after_stmt2)
  
  | Ast.While (exp, stmt) ->
      let test_label = new_label () in
      let top_label = new_label () in
      let exp_code, env_after_exp = compile_exp exp env in
      let stmt_code, env_after_stmt = compile_stmt stmt env_after_exp in
      ([Mips.Label test_label] @ exp_code @
       [Mips.Beq (Mips.R2, Mips.R0, top_label)] @
       stmt_code @ [Mips.J test_label; Mips.Label top_label], env_after_stmt)
  
  | Ast.For (init, cond, iter, stmt) ->
      let init_code, env_after_init = compile_exp init env in
      let iter_code, env_after_iter = compile_exp iter env in
      let cond_code, env_after_cond = compile_exp cond env in
      let stmt_code, env_after_stmt = compile_stmt stmt env in
      let top_label = new_label () in
      let test_label = new_label () in
      (init_code @ [Mips.Label top_label] @ cond_code @
       [Mips.Beq (Mips.R2, Mips.R0, test_label)] @
       stmt_code @ iter_code @
       [Mips.J top_label; Mips.Label test_label], env_after_stmt)
  
  | Ast.Return exp ->
      let exp_code, updated_env = compile_exp exp env in
      (exp_code, updated_env)
  
  | Ast.Let (var, exp, stmt) ->
    let exp_code, env_with_exp = compile_exp exp env in  (* Compile the initializing expression *)
    let offset = env_with_exp.stack_offset - 4 in  (* Calculate new variable's stack offset *)
    let store_code = [Mips.Sw (Mips.R2, Mips.R30, Word32.fromInt offset)] in  (* Store result of exp in new var's location *)
    let updated_env = { varmap = (var, offset) :: env_with_exp.varmap;  (* Add new var to environment *)
                        stack_offset = offset } in  (* Update stack offset in environment *)
    let stmt_code, final_env = compile_stmt stmt updated_env in  (* Compile the statement with updated environment *)
    (exp_code @ store_code @ stmt_code, final_env)  (* Combine codes and return final environment *)

  | _ -> raise IMPLEMENT_ME



let compile_func func =
  let funcsig = match func with
    | Ast.Fn f -> f
  in
  let num_saved_registers = 2 in
  let num_local_vars = 20 in
  let frame_size = (num_local_vars + num_saved_registers) * 4 in

  (* Adjust initial_env to include argument offsets correctly, assuming all arguments are on the stack *)
  let initial_varmap, args_setup_code, stack_offset =
    let rec create_varmap_and_setup args stack_offset varmap setup_code counter =
      match args with
      | [] -> varmap, setup_code, stack_offset
      | arg :: rest ->
        (* Each argument is above the frame pointer in the caller's frame, adjust for saved registers *)
        let arg_offset_from_fp = -(4 * (counter + num_saved_registers + 1)) in
        let move_arg_code = 
          (* Move argument from caller's stack position to current stack position *)
          let caller_offset = 4 * counter in  (* Adjust based on current argument's position in caller's stack *)
          [Mips.Lw (Mips.R9, Mips.R30, Word32.fromInt caller_offset);
            Mips.Sw (Mips.R9, Mips.Fp, Word32.fromInt arg_offset_from_fp)]
        in
        create_varmap_and_setup rest (stack_offset - 4) ((arg, arg_offset_from_fp) :: varmap) (setup_code @ move_arg_code) (counter + 1)
    in
    create_varmap_and_setup funcsig.args 0 [] [] 0
  in
  let initial_env = { varmap = initial_varmap; stack_offset = stack_offset } in

  let entry_label = funcsig.name in

  let prologue = [
    Mips.Label entry_label;
    Mips.Subu (Mips.R29, Mips.R29, Word32.fromInt frame_size);
    Mips.Sw (Mips.R30, Mips.R29, Word32.fromInt (frame_size - 4));
    Mips.Sw (Mips.R31, Mips.R29, Word32.fromInt (frame_size - 8));
    Mips.Addu (Mips.R30, Mips.R29, Word32.fromInt frame_size);
  ] in

  let body_code, _ = compile_stmt funcsig.body initial_env in

  let epilogue_label = "end_" ^ funcsig.name in
  let epilogue = [
    Mips.Label epilogue_label;
    Mips.Lw (Mips.R30, Mips.R29, Word32.fromInt (frame_size - 4));
    Mips.Lw (Mips.R31, Mips.R29, Word32.fromInt (frame_size - 8));
    Mips.Addu (Mips.R29, Mips.R29, Word32.fromInt frame_size);
    Mips.Jr (Mips.R31);
  ] in

  prologue @ args_setup_code @ body_code @ epilogue


          

let rec compile (p:Ast.program) : result =
  (* Generate code for all functions *)
  let funcs_code = List.flatten (List.map compile_func p) in

  (* Add a bootstrap to jump to main *)
  let bootstrap = [Mips.J "main"] in

  (* Combine bootstrap, function code, and maybe a data section *)
  let complete_code = bootstrap @ funcs_code in
  (* TODO: Handle the global data section *)
  let data_section = [] (* Handle global data if necessary *)
  in 
  { code = complete_code; data = data_section }

let result2string (res:result) : string = 
    let code = res.code in
    let data = res.data in
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let vaR8decl x = x ^ ":\t.word 0\n" in
    let readfile f =
      let stream = open_in f in
      let size = in_channel_length stream in
      let text = Bytes.create size in
      let _ = really_input stream text 0 size in
		  let _ = close_in stream in 
      text in
	  let debugcode = readfile "print.asm" in
	    "\t.text\n" ^
	    "\t.align\t2\n" ^
	    "\t.globl main\n" ^
	    (String.concat "" strs) ^
	    "\n\n" ^
	    "\t.data\n" ^
	    "\t.align 0\n"^
	    (String.concat "" (List.map vaR8decl data)) ^
	    "\n" ^
	    Bytes.to_string debugcode
