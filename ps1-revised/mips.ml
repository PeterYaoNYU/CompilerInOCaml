open Mips_ast
open Mips_assem
open Mips_sim
open Byte
open Format

let parse_file() =
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Parse.program Lex.lexer (Lexing.from_channel ch)

let parse_and_run() =
  let prog = parse_file() in
  let state' = interp (assem prog) in
  let _ = print_string ("Register File\n"^(string_of_rf state'.r)) in
  let _ = print_string ("Memory\n"^(string_of_mem state'.m)) in
  print_string ("PC = "^(Int32.to_string state'.pc)^"\n")
  
(* interpreter tests *)
let i_arith = [Li(R1,0x4l); Add(R2,R1,R1)]
let i_li = [Li(R1,0xFEEDFACEl)]
let i_mem = [Li(R15,0x20l); Sw(R15,R0,0x100l); Lw(R16,R0,0x100l)]
let i_beqt = [Beq(R0,R0,0x4l)]
let i_beqf = [Li(R1,0x4l); Beq(R0,R1,0x4l)]
let i_jr = [Li(R1,0xFEE0l); Jr(R1)]
let i_jal = [Jal(0xFEE0l)]

let test_reg rf expected =
  List.map (fun (r,v) -> let v' = rf_lookup (reg2ind r) rf in
	      if v' = v || v' = (rev_endianess v)
	      then ()
	      else print_string ((format_string "failed (expected,got): " Bright Red)^(tostring v)^", "^(tostring v')^"\n")) expected

let test_mem mem expected =
  List.map (fun (a,v) -> let v' = read_word mem a in
	      if v' = v || v' = (rev_endianess v)
	      then ()
	      else print_string ((format_string "failed (expected,got): " Bright Red)^(tostring v)^", "^(tostring v')^"\n")) expected

let test_pc pc1 pc2 (f,v) =
  match f with
    1 -> if (Int32.add pc1 v) = pc2 
         then print_string (format_string "PC passed\n" Bright Green)
         else print_string (format_string "PC failed\n" Bright Red)
  | _ -> if pc2 = v 
         then print_string (format_string "PC passed\n" Bright Green)
         else print_string (format_string "PC failed\n" Bright Red)

let testi str i e1 e2 e3 =
  (try (
  let _ = print_endline str in
  let state = assem i in
  (* let _ = print_string (string_of_rf (state.r)) in
  let _ = print_string (string_of_mem (state.m)) in *)
  let state' = interp state in
  (* let _ = print_string (string_of_rf (state'.r)) in
  let _ = print_string (string_of_mem (state'.m)) in
  let _ = print_string ((tostring state'.pc)^"\n") in *)
  let _ = print_string ("testing register file ...\n") in
  let _ = test_reg state'.r e1 in
  let _ = print_string ("testing memory ... \n") in
  let _ = test_mem state'.m e2 in
  let _ = print_string ((tostring state.pc)^"\n") in
  let _ = print_string ((tostring state'.pc)^"\n") in
  let _ = test_pc state.pc state'.pc e3 in
  ()
  ) with _ -> (print_string "runtime failure\n"))


let test_interpreter () =
  let _ = testi "i_arith" i_arith [(R1,0x4l); (R2,0x8l)] [] (1,0xCl) in
  let _ = testi "i_li" i_li [(R1,0xFEEDFACEl)] [] (1,0x8l) in
  let _ = testi "i_mem" i_mem [(R15,0x20l); (R16,0x20l)] [(0x100l,0x20l)] (1,0x10l) in
  let _ = testi "i_beqt" i_beqt [] [] (1,0x10l) in
  let _ = testi "i_beqf" i_beqf [(R1,0x4l)] [] (1,0xCl) in
  let _ = testi "i_jr" i_jr [(R1,0xFEE0l)] [] (0,0xFEE0l) in
  let _ = testi "i_jal" i_jal [] [] (0,0x3FB80l) in
  ()
;;

(* Expect either 0 or 1 command line argument. If 1, it is the file to interpret. Otherwise,
   just run the test suite.

 * usage: ps1 [file-to-interpret] *)

let _ =
  if Array.length Sys.argv = 1 then
    test_interpreter ()
  else
    parse_and_run ()
    
