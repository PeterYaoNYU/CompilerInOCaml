(* Compile Fish AST to MIPS AST *)
open Mips

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty)

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else (variables := VarSet.add t (!variables); t)

(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty)


(* Helper function to collect variables inside an expression *)
let rec collect_vars_exp ((e, _) : Ast.exp) : unit =
    match e with
    | Ast.Int _ -> ()  (* Integers are not variables, do nothing *)
    | Ast.Var v -> variables := VarSet.add ("var_" ^ v) (!variables)  (* Add the variable to the set *)
    | Ast.Binop (e1, _, e2) ->
        collect_vars_exp e1;  (* Collect variables in the first expression *)
        collect_vars_exp e2   (* Collect variables in the second expression *)
    | Ast.Not exp -> collect_vars_exp exp  (* Collect variables in the negated expression *)
    | Ast.And (exp1, exp2) | Ast.Or (exp1, exp2) ->
        collect_vars_exp exp1;  (* Collect variables in the first operand *)
        collect_vars_exp exp2   (* Collect variables in the second operand *)
    | Ast.Assign (v, exp) ->
        variables := VarSet.add ("var_" ^ v) (!variables);  (* Add the variable being assigned to to the set *)
        collect_vars_exp exp  (* Collect variables in the expression being assigned *)


(* helper function: collect variables inside a single statment *)
let rec collect_vars_stmt ((s, _) : Ast.stmt) : unit =
    match s with
    | Ast.Exp exp -> collect_vars_exp exp  (* Collect variables in the expression *)
    | Ast.Seq (stmt1, stmt2) ->
        collect_vars_stmt stmt1;  (* Collect variables in the first statement *)
        collect_vars_stmt stmt2   (* Collect variables in the second statement *)
    | Ast.If (exp, stmt1, stmt2) ->
        collect_vars_exp exp;  (* Collect variables in the condition *)
        collect_vars_stmt stmt1;  (* Collect variables in the 'then' branch *)
        collect_vars_stmt stmt2   (* Collect variables in the 'else' branch *)
    | Ast.While (exp, stmt) ->
        collect_vars_exp exp;  (* Collect variables in the condition *)
        collect_vars_stmt stmt  (* Collect variables in the body *)
    | Ast.For (init, cond, iter, stmt) ->
        collect_vars_exp init;  (* Collect variables in the initialization *)
        collect_vars_exp cond;  (* Collect variables in the condition *)
        collect_vars_exp iter;  (* Collect variables in the iteration step *)
        collect_vars_stmt stmt  (* Collect variables in the body *)
    | Ast.Return exp -> collect_vars_exp exp  (* Collect variables in the return expression *)

(* find all of the variables in a program and add them to
 * the set variables *)
let rec collect_vars (p : Ast.program) : unit = 
      (* Collect variables in the statement *)
        collect_vars_stmt p

let rec compile_exp (exp:Ast.exp) : Mips.inst list =
    match exp with
    | Int n, _ -> [Mips.Li (Mips.R2, Word32.fromInt n)]  (* Load immediate value into R2 *)
    | Var v, _ -> [Mips.La (Mips.R2, ("var_" ^ v)); Mips.Lw(Mips.R2, Mips.R2, Word32.fromInt 0)]  (* Load variable value into R2 *)
    | Binop (e1, op, e2), _ -> 
        (
            let t = new_temp() in
            (compile_exp e1) @ [Mips.La (Mips.R8, t); Mips.Sw (Mips.R2, Mips.R8, Word32.fromInt 0)] @  (* Store R2 in a temporary variable *)
            (compile_exp e2) @ [Mips.La (Mips.R8, t); Mips.Lw (Mips.R8, Mips.R8, Word32.fromInt 0)] @  (* Load the temporary variable into R8 *)
            (match op with
                | Ast.Plus -> [Mips.Add (Mips.R2, Mips.R2, Reg Mips.R8)]  (* Add R8 to R2 and store the result in R2 *)
                | Ast.Minus -> [Mips.Sub (Mips.R2, Mips.R8, Mips.R2)]  (* Subtract R8 from R2 and store the result in R2 *)
                | Ast.Times -> [Mips.Mul (Mips.R2, Mips.R2, Mips.R8)]  (* Multiply R2 by R8 and store the result in R2 *)
                | Ast.Div -> [Mips.Div (Mips.R2, Mips.R8, Mips.R2)]  (* Divide R2 by R8 *)
                | Ast.Lt -> [Mips.Slt (Mips.R2, Mips.R8, Reg Mips.R2)]  (* Set R2 to 1 if R2 is less than R8, otherwise set it to 0 *)
                | Ast.Gt -> [Mips.Slt (Mips.R2, Mips.R2, Mips.R8)]  (* Set R2 to 1 if R8 is less than R2, otherwise set it to 0 *)
                | Ast.Gte -> 
                    let temp = new_temp() in  (* Temporary label for the inverse of lt *)
                    [Mips.Slt (Mips.R2, Mips.R8, Mips.R2); Mips.Xor(Mips.R2, Mips.R2, Immed (Word32.fromInt 1))]  (* Set R2 to 1 if R8 is less than R2, otherwise set it to 0 *)
                | Ast.Lte -> 
                    let temp = new_temp() in  (* Temporary label for the inverse of gt *)
                    [Mips.Slt (Mips.R2, Mips.R2, Mips.R8); Mips.Xor(Mips.R2, Mips.R2, Immed (Word32.fromInt 1))]  (* Set R2 to 1 if R8 is less than R2, otherwise set it to 0 *)
                | Ast.Eq -> 
                    [Mips.Seq (Mips.R2, Mips.R2, Mips.R8)]  (* Set R2 to 1 if R2 is equal to R8 *)
                | Ast.Neq ->
                    [Mips.Sne (Mips.R2, Mips.R2, Mips.R8)]  (* Set R2 to 1 if R2 is not equal to R8 *)
            )
        )
    | Not e, _ -> 
        (
            let label_true = new_label () in
            let label_end = new_label () in
            (compile_exp e) @ [Mips.Beq (Mips.R2, Mips.R0, label_true)] @  (* Jump to label_true if R2 is 0 *)
            [Mips.Li (Mips.R2, Word32.fromInt 0); Mips.J label_end; Mips.Label label_true] @  (* Load 0 into R2 and jump to label_end *)
            [Mips.Li (Mips.R2, Word32.fromInt 1); Mips.Label label_end]  (* Load 1 into R2 *)
        )
    | And (e1, e2), _ -> 
        (
            let label_false = new_label () in
            let label_end = new_label () in
            (compile_exp e1) @ [Mips.Beq (Mips.R2, Mips.R0, label_false)] @  (* Jump to label_false if R2 is 0 *)
            (compile_exp e2) @ [Mips.Beq (Mips.R2, Mips.R0, label_false)] @  (* Jump to label_false if R2 is 0 *)
            [Mips.Li (Mips.R2, Word32.fromInt 1); Mips.J label_end; Mips.Label label_false] @  (* Load 1 into R2 and jump to label_end *)
            [Mips.Li (Mips.R2, Word32.fromInt 0); Mips.Label label_end]  (* Load 0 into R2 *)
        )
    | Or (e1, e2), _ ->
        (
            let label_true = new_label () in
            let label_end = new_label () in
            (compile_exp e1) @ [Mips.Bne (Mips.R2, Mips.R0, label_true)] @  (* Jump to label_true if R2 is not 0 *)
            (compile_exp e2) @ [Mips.Bne (Mips.R2, Mips.R0, label_true)] @  (* Jump to label_true if R2 is not 0 *)
            [Mips.Li (Mips.R2, Word32.fromInt 0); Mips.J label_end; Mips.Label label_true] @  (* Load 0 into R2 and jump to label_end *)
            [Mips.Li (Mips.R2, Word32.fromInt 1); Mips.Label label_end]  (* Load 1 into R2 *)
        )
    | Assign (v, e), _ -> 
        (
            let t = new_temp() in
            (compile_exp e) @ [Mips.La (Mips.R8, ("var_" ^ v)); Mips.Sw (Mips.R2, Mips.R8, Word32.fromInt 0)]  (* Store R2 in the variable *)
        )

(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let rec compile_stmt ((s,_):Ast.stmt) : Mips.inst list = 
    match s with
    | Ast.Exp exp -> compile_exp exp  (* Placeholder for expression compilation function *)
    | Ast.Seq (stmt1, stmt2) ->
        compile_stmt stmt1 @ compile_stmt stmt2  (* Concatenate the instructions from both statements *)
    | Ast.If (exp, stmt1, stmt2) ->
        let else_l = new_label () in
        let end_l = new_label () in
        (compile_exp exp) @ [Mips.Beq (Mips.R2, Mips.R0, else_l)] @
        (compile_stmt stmt1) @ [Mips.J end_l; Mips.Label else_l] @
        (compile_stmt stmt2) @ [Mips.Label end_l]
    | Ast.While (exp, stmt) ->
        let test_l = new_label () in
        let top_l = new_label () in
        [Mips.J test_l; Mips.Label top_l] @
        (compile_stmt stmt) @
        [Mips.Label test_l] @ (compile_exp exp) @
        [Mips.Bne (Mips.R2, Mips.R0, top_l)]
    | Ast.For (init, cond, iter, stmt) ->
        let init_stmt = Ast.Exp init in
        let iter_stmt = Ast.Exp iter in
        let while_stmt = Ast.While (cond, (Ast.Seq (stmt, (iter_stmt, 0)), 0)) in
        compile_stmt (Ast.Seq ((init_stmt, 0), (while_stmt, 0)), 0)
    | Ast.Return exp ->
        compile_exp exp @ [Mips.Jr Mips.R31]  (* Compile the expression and then jump to return address *)
    | _ -> raise IMPLEMENT_ME  (* Other cases to be implemented *)



(* compiles Fish AST down to MIPS instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "fish_main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }


(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run within the SPIM simulator
 * (hopefully). *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl printInt\n" ^
    "\t.globl fish_main\n" ^
    "\t.globl main\n\n" ^
    "main:\n" ^
    "\tmove $s8, $31\n" ^
    "\tjal fish_main\n" ^
    "\tmove $31, $s8\n" ^
    "\tmove $a0, $2\n" ^
    "\tj printInt\n\n" ^
    "printInt:\n" ^
    "\tadd $t0, $v0, $zero\n"^
    "\tli $v0, 1\n"^
    "\tsyscall\n"^
    "\tadd $v0, $t0, $zero\n"^
    "\tjr $ra\n\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"
