open Mlish_ast

(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)

type mode = Typecheck | Compile

let mode_of_opt str =
  if str = "typecheck" then
    Typecheck
  else
    Compile

let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if (Array.length argv != 3 || (not (argv.(1) = "typecheck" || argv.(1) = "compile")))
    then (prerr_string ("usage: " ^ argv.(0) ^ " [typecheck or compile] [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(2) in
  (mode_of_opt argv.(1), Ml_parse.program Ml_lex.lexer (Lexing.from_channel ch))

let type_prog prog =
  try
    let ty = Mlish_type_check.type_check_exp prog in
    print_string (tipe2string ty)
  with
    Mlish_type_check.TypeError -> print_string "\nType error"

let compile_prog prog = 
  let _ = Mlish_type_check.type_check_exp prog in
  Mlish_compile.compile_exp prog

let run_prog prog = Scish_eval.run prog

let _ = 
  let (mode, prog) = parse_file() in
  match mode with
  | Typecheck ->
     type_prog prog
  | Compile ->
     let prog' = compile_prog prog in
     let ans = run_prog prog' in
     print_string ("answer = "^(Scish_eval.val2string ans)^"\n")

(*
let dump p = print_string (Cish_ast.prog2string p)

let _ =
  let prog = parse_file() in
 let ans = run_prog prog in
(*
let _ =  print_string ("answer = "^(string_of_int ans)^"\n") in
*)
  dump (compile_prog prog)
*)
