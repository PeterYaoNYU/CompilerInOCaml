type token =
  | INT of (int)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parse.mly"
open Ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
# 19 "parse.ml"
let yytransl_const = [|
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\003\000\000\000\001\000"

let yydgoto = "\002\000\
\003\000\004\000"

let yysindex = "\255\255\
\000\000\000\000\000\000\001\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000"

let yytablesize = 1
let yytable = "\001\000\
\005\000"

let yycheck = "\001\000\
\000\000"

let yynames_const = "\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    Obj.repr(
# 41 "parse.mly"
           ( _1 )
# 71 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parse.mly"
              ( (Ast.skip, 0) )
# 77 "parse.ml"
               : Ast.stmt))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
