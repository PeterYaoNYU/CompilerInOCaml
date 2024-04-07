open Mlish_ast

exception TypeError
(* let type_error(s:string) = (print_string s; raise TypeError) *)
let type_error(s:string) = (raise TypeError)


let type_variable_counter = ref 0


let rec expr2string ((e, _): exp) : string =
  match e with
  | Var var -> "Var " ^ var
  | PrimApp (prim, exps) -> "PrimApp (" ^ prim2string prim ^ ", [" ^ String.concat "; " (List.map expr2string exps) ^ "])"
  | Fn (v, e) -> "Fn (" ^ v ^ ", " ^ expr2string e ^ ")"
  | App (e1, e2) -> "App (" ^ expr2string e1 ^ ", " ^ expr2string e2 ^ ")"
  | If (e1, e2, e3) -> "If (" ^ expr2string e1 ^ ", " ^ expr2string e2 ^ ", " ^ expr2string e3 ^ ")"
  | Let (v, e1, e2) -> "Let (" ^ v ^ ", " ^ expr2string e1 ^ ", " ^ expr2string e2 ^ ")"
and prim2string prim = 
  match prim with
  | Int n -> "Int " ^ string_of_int n
  | Bool b -> "Bool " ^ string_of_bool b
  | Unit -> "Unit"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Div -> "Div"
  | Eq -> "Eq"
  | Lt -> "Lt"
  | Pair -> "Pair"
  | Fst -> "Fst"
  | Snd -> "Snd"
  | Nil -> "Nil"
  | Cons -> "Cons"
  | IsNil -> "IsNil"
  | Hd -> "Hd"
  | Tl -> "Tl"

(* Function to print an expression *)
let print_expr expr =
  print_endline (expr2string expr);;


let rec tipe_to_string (t: tipe) : string =
  match t with
  | Tvar_t x -> "'" ^ x
  | Guess_t t ->
    (
      match !t with 
      | None -> "guess None"
      | Some t' ->"guess" ^ (tipe_to_string t')
    )
  | Int_t -> "int"
  | Bool_t -> "bool"
  | Unit_t -> "unit"
  | Fn_t(t1, t2) ->
      "(" ^ tipe_to_string t1 ^ " -> " ^ tipe_to_string t2 ^ ")"
  | Pair_t(t1, t2) ->
      "(" ^ tipe_to_string t1 ^ " * " ^ tipe_to_string t2 ^ ")"
  | List_t t ->
      "(" ^ tipe_to_string t ^ " list)"

let print_tipe (t: tipe) : unit = 
  print_endline (tipe_to_string t)

(* for the generalize function *)
let freshvar_counter = ref 0
let freshvar () =
  let count = !freshvar_counter in
  freshvar_counter := count + 1;
  "t" ^ string_of_int count

(* let guess() : tipe = 
  let current_count = !type_variable_counter in
  type_variable_counter := current_count + 1;
  print_string ("g" ^ (string_of_int current_count) ^ "\n");
  Tvar_t ("g" ^ (string_of_int current_count)) *)

let guess() = Guess_t (ref None)

(* b is a list of pairs, with such formation: (generic type, guess()) *)
(* should replace every generic type in t that appears in b with the guess in b *)
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
  (* print_endline "In instantiate"; *)
  match s with
  | Forall([], t) -> t
  | Forall(vs, t) ->
    (* print_endline "Variables in vs:"; *)
    (* List.iter (fun var_name -> print_endline var_name) vs; *)
    (* print_endline  "End of vs"; *)
    let b = List.map (fun a -> (a, guess())) vs in
    substitute b t 

(* helpers for the generalize implementation *)
let rec guesses_of_type t = match t with
  | Tvar_t x -> []
  | Guess_t r -> 
    begin match !r with
    | Some t' -> guesses_of_type t'
    | None -> 
      (* let new_var = freshvar () in
      r := Some (Tvar_t new_var);  *)
      [r]
    end
  | Fn_t (t1, t2) | Pair_t (t1, t2) -> List.append (guesses_of_type t1) (guesses_of_type t2)
  | List_t t' -> guesses_of_type t'
  | Int_t | Bool_t | Unit_t -> []

let minus lst1 lst2 =
  List.filter (fun x -> not (List.mem x lst2)) lst1 

  (* subs is a list of pairs, the fst of the pairs in tipe t needs to be changed to the snds *)
let rec subst_guess subs t = match t with
  | Tvar_t tv -> t
  | Guess_t r ->
    (
      match !r with
      | Some t' -> subst_guess subs t'
      | None -> 
        let new_t = List.assoc r subs in
        r := Some  (Tvar_t new_t);
        Tvar_t new_t
    )
  | Fn_t (t1, t2) -> Fn_t (subst_guess subs t1, subst_guess subs t2)
  | Pair_t (t1, t2) -> Pair_t (subst_guess subs t1, subst_guess subs t2)
  | List_t t' -> List_t (subst_guess subs t')
  | Int_t | Bool_t | Unit_t as non_var_t -> non_var_t

let print_string_list l =
  List.iter (fun item -> print_endline item) l;
  print_string "\n";;

let generalize env t =
  (* print_endline "In generalize"; *)
  (* print_tipe t; *)
  let t_gs = guesses_of_type t in
  (* print_string "t_gs: ";
  print_int (List.length t_gs); *)
  (* print_string_list t_gs; *)
  (* print_endline "End of t_gs"; *)

  (* let env_bound_vars = (* Collect all bound type variables from the environment *)
    List.fold_left (fun acc (_, scheme) ->
      match scheme with
      | Forall(vs, _) -> List.append acc vs
      | _ -> acc
    ) [] env in *)
  
  let env_bound_vars = [] in


  (* let env_gs = List.fold_left (fun acc gs -> List.append acc gs) [] env_list_gs in *)
  let diff = minus t_gs env_bound_vars in
  (* print_string "diff: "; *)
  (* print_string_list diff; *)
  (* print_endline "End of diff"; *)
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

  let rec occurs_check r t = 
    match t with
    | Tvar_t x -> 
      (match !r with
       | Some Tvar_t y -> x = y
       | _ -> false)
    | Fn_t(t1, t2) | Pair_t(t1, t2) -> occurs_check r t1 || occurs_check r t2
    | List_t t' -> occurs_check r t'
    | Guess_t ref_t -> 
      if r == ref_t then true (* Direct self-reference *)
      else 
        (match !ref_t with
         | Some t' -> occurs_check r t' (* Indirect self-reference *)
         | None -> false)
    | Int_t | Bool_t | Unit_t -> false

let rec unify (t1: tipe) (t2: tipe): bool =
  (* print_string "In unify, and types are: \n"; 
  print_tipe t1; print_tipe t2;
  print_endline "End of unify"; *)
  (* print_endline "End of unify"; *)
  if (is_equal t1 t2) then true else
  match t1, t2 with 
  | Guess_t r, _ when occurs_check r t2 -> false
  | Guess_t r, _ -> 
    (* print_endline "Unifying Guess_t"; *)
    (* print_tipe t2; *)
    (match !r with
    | Some t1' -> unify t1' t2
    | None -> r := Some t2; true
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

let rec tc (env: (var * tipe_scheme) list) (e: exp) =
  (* print_endline (expr2string e);  *)
  match e with 
  | Var x, _ -> instantiate (lookup env x)
  | PrimApp (prim, exp_list), _ -> 
    (match prim, exp_list with
    | Int int, [] -> Int_t
    | Bool bool, [] -> Bool_t
    | Unit, [] -> Unit_t
    | Plus, [e1; e2]
    | Minus, [e1; e2]
    | Times, [e1; e2] ->
        let t1 = tc env e1 in
        (* print_tipe t1; *)
        let t2 = tc env e2 in
        if (unify t1 Int_t) && (unify t2 Int_t) then Int_t else type_error "Arithmetic operation failed"
    | Eq, [e1; e2] -> 
        let t1 = tc env e1 in
        let t2 = tc env e2 in
        if (unify t1 Int_t) && (unify t2 Int_t) then Bool_t else type_error "Equality operation failed"
    | Lt, [e1; e2] -> 
        let t1 = tc env e1 in
        let t2 = tc env e2 in
        if (unify t1 Int_t) && (unify t2 Int_t) then Bool_t else type_error "Less than operation failed"
    | Fst, [e1] ->
      let t1 = tc env e1 in
      (* print_tipe t1; *)
      (
        match t1 with
          | Pair_t (t, _) -> 
            t
          | Guess_t r -> 
            (
              match !r with
              | Some Pair_t (t1, t2) -> t1
              | None ->
                (* print_endline "In Fst guess"; *)
                let g1 = guess() in
                let g2 = guess() in
                r:= Some (Pair_t (g1, g2));
                (* print_endline "return g1 in Fst guess"; *)
                g1
              | _ -> type_error "Fst applied to non-pair, fail in guess"
            )
          | _ -> type_error "Fst applied to non-pair"
      )
    | Snd, [e1] ->
      let t1 = tc env e1 in
        (
          match t1 with
            | Pair_t (_, t) -> t
            | Guess_t r -> 
              (
              match !r with
                | Some Pair_t (t1, t2) -> t2
                | None ->
                  (* print_endline "In Snd guess"; *)
                  let g1 = guess() in
                  let g2 = guess() in
                  r:= Some (Pair_t (g1, g2));
                  g2
                | _ -> type_error "Snd applied to non-pair, fail in guess"
              )
            | _ -> type_error "Snd applied to non-pair"
        )
    | Cons, [e1; e2] ->
      let t1 = tc env e1 in
      let t2 = tc env e2 in
      (
        match t2 with
        | List_t t -> if unify t1 t then List_t t else type_error "Cons type mismatch"
        | Guess_t _ -> List_t t1
        | _ -> type_error "Cons applied to non-list"
      )
    | Pair, [e1; e2] ->
      let t1 = tc env e1 in
      let t2 = tc env e2 in
      Pair_t (t1, t2)
    | Nil, [] -> 
      let g = guess() in List_t g
    | IsNil, [e1] ->
      let t1 = tc env e1 in
      (
        match t1 with
        | List_t _ -> Bool_t
        | Guess_t _ -> unify t1 (List_t (guess())) ; Bool_t
        | _ -> type_error "IsNil applied to non-list"
      )
    | Hd, [e1] ->
        let t1 = tc env e1 in
        (match t1 with
        | List_t t -> t
        | Guess_t r -> 
          (
            match !r with
              | Some List_t t'-> t' 
              | Some t' -> t'
              | None -> 
                let g = guess() in
                if unify t1 (List_t g) then g else type_error "Hd guess failed"
          )
        | _ -> type_error "Hd applied to non-list")
    | Tl, [e1] ->
        let t1 = tc env e1 in
        (match t1 with
        | List_t _ -> t1
        | Guess_t r -> 
          (
            match !r with
              | Some t' -> t'
              | None ->
                let g = guess() in
                if unify t1 (List_t g) then g else type_error "Tl guess failed"
          )
        | _ -> type_error "Tl applied to non-list")
    | _, _ -> type_error "Invalid primitive application")
  | Fn(x, e), _ -> 
    let g = guess() in
    Fn_t (g, tc ((x, Forall([], g))::env) e)
  | App(e1, e2), _ ->
    let t1 = tc env e1 in
    let t2 = tc env e2 in
    let t = guess() in
    if (unify t1 (Fn_t(t2,t))) then t else type_error "App failed"
  | Let(x, e1, e2), _ ->
    let s = generalize env (tc env e1) in 
    (* print_endline "finish generalize"; *)
    tc ((x, s)::env) e2 
  | If (e1, e2, e3), _ ->
    let t1 = tc env e1 in
    let t2 = tc env e2 in
    let t3 = tc env e3 in
    if (unify t1 Bool_t) && (unify t2 t3) then t2 else type_error "If failed"


let type_check_exp (e:Mlish_ast.exp) : tipe =
  tc [] e