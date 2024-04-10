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

(* key idea to represent Nil: use the first element of the list to store the length *)


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
      | ML.IsNil, [(e1, pos)]-> S.If (S.PrimApp (S.Eq, [S.PrimApp(S.Fst, [compile_exp_aux (e1, pos)])  ; S.Int 0]),
                                       S.Int 1, S.Int 0)
      | ML.Cons, [e1; e2] ->
        S.PrimApp (S.Cons, [S.PrimApp(S.Plus, [S.Int 1; S.PrimApp(S.Fst, [compile_exp_aux e2])]);
                             S.PrimApp(S.Cons, [compile_exp_aux e1 ; 
                             S.PrimApp(S.Snd, [compile_exp_aux e2])])])
      | ML.Hd, [e] -> S.If (S.PrimApp(S.Eq, [S.PrimApp(S.Fst, [compile_exp_aux e]); S.Int 0]), 
                            (compile_exp_aux e), 
                            S.PrimApp(S.Fst, [S.PrimApp(S.Snd, [compile_exp_aux e])]))
      | ML.Tl, [e] -> S.If (S.PrimApp(S.Eq, [S.PrimApp(S.Fst, [compile_exp_aux e]); S.Int 0]), 
                            S.PrimApp (S.Cons, [S.Int 0; S.Int 0]), 
                            (S.PrimApp(S.Cons, [S.PrimApp(S.Minus, [S.PrimApp(S.Fst, [compile_exp_aux e]); S.Int 1]);
                                          S.PrimApp(S.Snd, [S.PrimApp(S.Snd, List.map compile_exp_aux es)])])))
      | ML.Nil, [] ->  S.PrimApp (S.Cons, [S.Int 0; S.Int 0])
      | _ -> S.PrimApp (compile_prim p, List.map compile_exp_aux es)
    )
  | ML.Fn (x, e) ->  S.Lambda (x, compile_exp_aux e)
  | ML.App (e1, e2) ->  S.App (compile_exp_aux e1, compile_exp_aux e2)
  | ML.If (e1, e2, e3) -> S.If (compile_exp_aux e1, compile_exp_aux e2, compile_exp_aux e3)
  | ML.Let (x, e1, e2) -> S.sLet x (compile_exp_aux e1) (compile_exp_aux e2)

let compile_exp ((e, pos): ML.exp) =
  (compile_exp_aux (e, pos))
  