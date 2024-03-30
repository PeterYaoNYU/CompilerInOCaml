open Mlish_ast

exception TypeError
let type_error(s:string) = (print_string s; raise TypeError)


let type_variable_counter = ref 0

(* for the generalize function *)
let freshvar_counter = ref 0
let freshvar () =
  let count = !freshvar_counter in
  freshvar_counter := count + 1;
  "t" ^ string_of_int count

let guess() : tipe = 
  let current_count = !type_variable_counter in
  type_variable_counter := current_count + 1;
  Tvar_t ("g" ^ (string_of_int current_count))

let rec substitute b t : tipe = 
  match t with 
  | Tvar_t x ->
    (
      try List.assoc x b with Not_found -> t
    )
  | Fn_t (b1, b2) ->
    Fn_t (substitute b b1, substitute b b2)
  | Pair_t (b1, b2) ->
    Pair_t (substitute b b1, substitute b b2)
  | List_t b1 ->
    List_t (substitute b b1)
  | Int_t | Bool_t | Unit_t -> t
  | Guess_t tr as guess -> 
    (
      match !tr with
      | None -> guess
      | Some t -> substitute b t
    )

let rec lookup env x = 
  match env with
  | [] -> raise TypeError
  | (var, scheme)::rest -> 
      if var = x then scheme 
      else lookup rest x
      
let instantiate s : tipe =
  match s with
  | Forall([], t) -> t
  | Forall(vs, t) ->
    let b = List.map (fun a -> (a, guess())) vs in
    substitute b t 

(* helpers for the generalize implementation *)
let rec guesses_of_type t = match t with
  | Tvar_t _ -> []
  | Guess_t r -> 
    begin match !r with
    | Some t' -> guesses_of_type t'
    | None -> [freshvar ()] (* Assign a fresh type variable to unresolved guess *)
    end
  | Fn_t (t1, t2) | Pair_t (t1, t2) -> List.append (guesses_of_type t1) (guesses_of_type t2)
  | List_t t' -> guesses_of_type t'
  | Int_t | Bool_t | Unit_t -> []

let minus lst1 lst2 =
  List.filter (fun x -> not (List.mem x lst2)) lst1 

let rec subst_guess subs t = match t with
  | Tvar_t tv -> (try List.assoc tv subs with Not_found -> t)
  | Guess_t r ->
    (
      match !r with
      | Some t' -> subst_guess subs t'
      | None -> (Guess_t r)
    )
  | Fn_t (t1, t2) -> Fn_t (subst_guess subs t1, subst_guess subs t2)
  | Pair_t (t1, t2) -> Pair_t (subst_guess subs t1, subst_guess subs t2)
  | List_t t' -> List_t (subst_guess subs t')
  | Int_t | Bool_t | Unit_t as non_var_t -> non_var_t

let generalize env t =
  let t_gs = guesses_of_type t in
  let env_list_gs = List.concat (List.map (fun (_, s) -> match s with
                                              | Forall (_, t) -> guesses_of_type t
                                              | _ -> []) env) in
  (* let env_gs = List.fold_left (fun acc gs -> List.append acc gs) [] env_list_gs in *)
  let diff = minus t_gs env_list_gs in
  let gs_vs = List.map (fun g -> (g, freshvar ())) diff in
  let tc = subst_guess gs_vs t in
  Forall (List.map snd gs_vs, tc)


(* End of generalization *)

let rec is_equal (t1:tipe) (t2:tipe):bool =
  match t1, t2 with
  | Tvar_t(tv1), Tvar_t(tv2) -> tv1 = tv2
  | Int_t, Int_t -> true
  | Bool_t, Bool_t -> true
  | Unit_t, Unit_t -> true
  | Fn_t(arg1, ret1), Fn_t(arg2, ret2) -> is_equal arg1 arg2 && is_equal ret1 ret2
  | Pair_t(t1a, t1b), Pair_t(t2a, t2b) -> is_equal t1a t2a && is_equal t1b t2b
  | List_t(t1), List_t(t2) -> is_equal t1 t2
  | Guess_t(t1'), Guess_t(t2') -> 
      (match !t1', !t2' with
        | Some t1'', Some t2'' -> is_equal t1'' t2''
        | None, None -> true
        | _, _ -> false)
  | _, _ -> false

let rec unify (t1: tipe) (t2: tipe): bool = 
  if (t1 == t2) then true else
  match t1, t2 with 
  | Guess_t r, _ -> 
    (match !r with
    | None -> r := Some t2; true
    | Some t1' -> unify t1' t2
    )
  | _, Guess_t _ -> unify t2 t1
  | Int_t, Int_t -> true
  | Bool_t, Bool_t -> true
  | Unit_t, Unit_t -> true
  | Fn_t(t1a, t1b), Fn_t(t2a, t2b) -> unify t1a t2a && unify t1b t2b
  | Pair_t(t1a, t1b), Pair_t(t2a, t2b) -> unify t1a t2a && unify t1b t2b
  | List_t(t1), List_t(t2) -> unify t1 t2
  | Tvar_t(tv1), Tvar_t(tv2) when tv1 = tv2 -> true
  | _, _ -> false

let rec tc (env: (varr * tipe_scheme) list) (e: exp) = 
  match e with 
  | Var x, _ -> instantiate (lookup env x)
  | PrimApp (prim, exp_list), _ -> 
    (match prim, exp_list with
    | Plus, [e1; e2]
    | Minus, [e1; e2] -> 
        let t1 = tc env e1 in
        let t2 = tc env e2 in
        if unify (t1, Int_t) && unify (t2, Int_t) then Int_t else type_error "Arithmetic operation failed"
    | Eq, [e1; e2] -> 
        let t1 = tc env e1 in
        let t2 = tc env e2 in
        if unify (t1, Int_t) && unify (t2, Int_t) then Bool_t else type_error "Equality operation failed"
    | _, _ -> type_error "Invalid primitive application")
  | Fn(x, e), _ -> 
    let g = guess() in
    Fn_t (g, tc ((x, Forall([], g))::env) e)
  | App(e1, e2), _ ->
    let t1 = tc env e1 in
    let t2 = tc env e2 in
    let t = guess() in
    if unify (t1, Fn_t(t2, t)) then t else type_error "App failed"
  | Let(x, e1, e2), _ ->
    let s = generalize(env, tc env e1) in 
    tc ((x, s)::env) e2 


let type_check_exp (e:Mlish_ast.exp) : tipe =
  tc [] e