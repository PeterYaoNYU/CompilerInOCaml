open Ast
open Eval
open Sexplib

(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)

let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if (Array.length argv != 3 || (not (argv.(1) = "src" || argv.(1) = "sexp")))
    then (prerr_string ("usage: " ^ argv.(0) ^ " [src or sexp] [file-to-compile]\n");
    exit 1) in
  if argv.(1) = "src" then
    let ch = open_in argv.(2) in
    Parse.program Lex.lexer (Lexing.from_channel ch)
  else
    let ch = open_in argv.(2) in
    let sexp = input_line ch in
    Ast.stmt_of_sexp (Sexp.of_string sexp)
    

let compile_prog prog =
  Compile.result2string (Compile.compile prog )

let parse_and_compile() =
  let prog = parse_file() in
  print_string (compile_prog prog)

let dump_sexp_prog ()=
  let prog = parse_file () in
  let sexp = Ast.sexp_of_stmt prog in
  print_string (Sexp.to_string sexp)

let eval_prog() =
  let prog = parse_file() in
  let ans = eval prog in
  print_string ("answer = "^(string_of_int ans)^"\n")

(* This program expects two arguments a "mode" (either src or sexp),
  and a file to compile.  Use parse_and_compile to print the generated
  mips code to stdout. Comment run_prog and uncomment eval_prog to get
  the evaluated answer instead. By default, this is set up to print mips
  code. Usage: ps3 [src or sexp] [file-to-parse] *)

let _ = parse_and_compile()
(* let _ = eval_prog() *)
