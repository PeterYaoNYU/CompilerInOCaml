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

let rec compile_exp ((e,pos):ML.exp) : S.exp =
  print_endline (ML.exp2string (e, pos)); 
  match e with
  | ML.Var var -> S.Var var
  | ML.PrimApp (p, es) ->
    print_endline "Compiling PrimApp...";
    (
      match p, es with 
      | ML.Int i, _ -> S.Int i
      | ML.Bool true, _ -> S.Int 1
      | ML.Bool false, _ -> S.Int 0
      | ML.IsNil, [e1]-> print_endline "checking Isnil: "; print_endline (ML.exp2string e1); S.Int 1
      | ML.Cons, [e1; e2] ->
        print_endline "Compiling Cons...";  
        (
          match e2 with
          | ML.PrimApp (ML.Nil, []), 0 -> compile_exp e1
          | _ -> S.PrimApp (S.Cons, [compile_exp e1])
        )
      | ML.Hd, [e] -> S.PrimApp (S.Fst, [compile_exp e])
      | ML.Tl, [e] -> S.PrimApp (S.Snd, [compile_exp e])  
      | ML.Nil, [] -> print_endline "Nil"; S.Int 0
      | _ -> S.PrimApp (compile_prim p, List.map compile_exp es)
    )
  | ML.Fn (x, e) -> print_endline "compiling function"; S.Lambda (x, compile_exp e)
  | ML.App (e1, e2) -> print_endline "App"; S.App (compile_exp e1, compile_exp e2)
  | ML.If (e1, e2, e3) -> S.If (compile_exp e1, compile_exp e2, compile_exp e3)
  | ML.Let (x, e1, e2) -> S.sLet x (compile_exp e1) (compile_exp e2)

