module ML = Mlish_ast
module S = Scish_ast

(* exception ImplementMe *)
exception UnavailType


let rec compile_prim (p:ML.prim) : S.primop = 
  match p with
  | ML.Plus -> S.Plus
  | ML.Minus -> S.Minus
  | ML.Times -> S.Times
  | ML.Div -> S.Div
  | ML.Eq -> S.Eq
  | ML.Lt -> S.Lt
  | ML.Pair -> S.Cons
  | ML.Fst -> S.Fst
  | ML.Snd -> S.Snd
  | ML.Cons -> S.Cons
  | _ -> raise UnavailType


let rec compile_exp_aux ((e,pos):ML.exp) : S.exp =
  (* print_endline (ML.exp2string (e, pos));  *)
  match e with
  | ML.Var var -> S.Var var
  | ML.PrimApp (p, es) ->
    (* print_endline "Compiling PrimApp..."; *)
    (
      match p, es with 
      | ML.Int i, _ -> S.Int i
      | ML.Bool true, _ -> S.Int 1
      | ML.Bool false, _ -> S.Int 0
      | ML.IsNil, [(e1, pos)]-> S.If (S.PrimApp (S.Eq, [compile_exp_aux (e1, pos); S.Var "()"]), S.Int 1, S.Int 0)
      | ML.Cons, [e1; e2] ->
        (* print_endline "Compiling Cons...";   *)
        (
          match e2 with
          | ML.PrimApp (ML.Nil, []), 0 -> compile_exp_aux e1
          | _ -> S.PrimApp (S.Cons, [compile_exp_aux e1; compile_exp_aux e2])
        )
      | ML.Hd, [e] -> S.PrimApp (S.Fst, [compile_exp_aux e])
      | ML.Tl, [e] -> S.PrimApp (S.Snd, [compile_exp_aux e])  
      | ML.Nil, [] ->  S.Var "()"
      | _ -> S.PrimApp (compile_prim p, List.map compile_exp_aux es)
    )
  | ML.Fn (x, e) ->  S.Lambda (x, compile_exp_aux e)
  | ML.App (e1, e2) ->  S.App (compile_exp_aux e1, compile_exp_aux e2)
  | ML.If (e1, e2, e3) -> S.If (compile_exp_aux e1, compile_exp_aux e2, compile_exp_aux e3)
  | ML.Let (x, e1, e2) -> S.sLet x (compile_exp_aux e1) (compile_exp_aux e2)

let compile_exp ((e, pos): ML.exp) =
  S.sLet "()" (S.Int 0) (compile_exp_aux (e, pos))
  