(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file() =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 3
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse] [output file]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel ch)

let parse_stdin() = 
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel stdin)

let process_fn fn =
  let curfblocks = (Cfg_ast.fn2blocks fn) in
  (match fn with
   | Fn {name; args; body; pos} -> print_string ("==========================\nProcessing function: " ^ name ^ "\n"));
  print_string ("blocks =\n" ^
    (Cfg_ast.fun2string curfblocks)^ "\n");
  print_string ("begin building interfere graph\n");
  let ig = Cfg.build_interfere_graph curfblocks in
  print_string ("Interference graph finished\n");
  let _ = print_string (Cfg.string_of_igraph ig) in
  Regalloc.reg_alloc curfblocks

let _ =
  let prog = parse_file() in
  let blocks = List.flatten (List.map (fun fn -> process_fn fn) prog) in
  let mipscode = Cfg_compile.cfg_to_mips blocks in
  let ch = open_out Sys.argv.(2) in
  let res = (Mips.prog2string mipscode) in
  print_newline ();
  print_string res;
  output_string ch res
