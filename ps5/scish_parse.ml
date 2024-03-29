type token =
  | IF
  | LPAREN
  | RPAREN
  | LAMBDA
  | EOF
  | LT
  | EQ
  | LET
  | LETREC
  | CONS
  | FST
  | SND
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | INT of (int)
  | ID of (string)

open Parsing;;
let _ = parse_error;;
# 2 "scish_parse.mly"
open Scish_ast
open Lexing
let parse_error s =
  let pos = Parsing.symbol_end_pos() in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n")
# 31 "scish_parse.ml"
let yytransl_const = [|
  257 (* IF *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LAMBDA *);
    0 (* EOF *);
  261 (* LT *);
  262 (* EQ *);
  263 (* LET *);
  264 (* LETREC *);
  265 (* CONS *);
  266 (* FST *);
  267 (* SND *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* TIMES *);
  271 (* DIV *);
    0|]

let yytransl_block = [|
  272 (* INT *);
  273 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\004\000\007\000\004\000\006\000\008\000\
\000\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\002\000\003\000\020\000\000\000\000\000\
\000\000\016\000\015\000\000\000\017\000\018\000\019\000\011\000\
\012\000\013\000\014\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\010\000\004\000\000\000\000\000\000\000\007\000\000\000\000\000\
\005\000\000\000\008\000"

let yydgoto = "\002\000\
\006\000\027\000\028\000\021\000"

let yysindex = "\002\000\
\255\254\000\000\036\255\000\000\000\000\000\000\004\000\255\254\
\003\255\000\000\000\000\004\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\255\254\255\254\000\000\255\254\247\254\
\248\254\007\255\255\254\008\255\255\254\009\255\255\254\000\000\
\000\000\000\000\010\255\255\254\011\255\000\000\014\255\255\254\
\000\000\015\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\255\000\000\000\000\000\000\
\000\000\000\000\017\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\250\255\000\000"

let yytablesize = 53
let yytable = "\007\000\
\003\000\020\000\001\000\022\000\024\000\025\000\023\000\030\000\
\031\000\032\000\034\000\036\000\038\000\040\000\004\000\005\000\
\041\000\043\000\026\000\009\000\033\000\029\000\000\000\000\000\
\000\000\000\000\000\000\035\000\000\000\037\000\000\000\000\000\
\000\000\000\000\039\000\000\000\008\000\003\000\042\000\009\000\
\010\000\011\000\012\000\000\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\004\000\005\000"

let yycheck = "\001\000\
\002\001\003\000\001\000\000\000\002\001\002\001\008\000\017\001\
\017\001\003\001\003\001\003\001\003\001\003\001\016\001\017\001\
\003\001\003\001\020\000\003\001\027\000\023\000\255\255\255\255\
\255\255\255\255\255\255\029\000\255\255\031\000\255\255\255\255\
\255\255\255\255\036\000\255\255\001\001\002\001\040\000\004\001\
\005\001\006\001\007\001\255\255\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001"

let yynames_const = "\
  IF\000\
  LPAREN\000\
  RPAREN\000\
  LAMBDA\000\
  EOF\000\
  LT\000\
  EQ\000\
  LET\000\
  LETREC\000\
  CONS\000\
  FST\000\
  SND\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Scish_ast.exp) in
    Obj.repr(
# 29 "scish_parse.mly"
          ( _1 )
# 146 "scish_parse.ml"
               : Scish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 32 "scish_parse.mly"
      ( Int(_1) )
# 153 "scish_parse.ml"
               : Scish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "scish_parse.mly"
     ( Var(_1) )
# 160 "scish_parse.ml"
               : Scish_ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Scish_ast.primop) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Scish_ast.exp list) in
    Obj.repr(
# 34 "scish_parse.mly"
                            ( PrimApp(_2,_3) )
# 168 "scish_parse.ml"
               : Scish_ast.exp))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Scish_ast.exp) in
    Obj.repr(
# 35 "scish_parse.mly"
                                            ( Lambda(_4,_6) )
# 176 "scish_parse.ml"
               : Scish_ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Scish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Scish_ast.exp) in
    Obj.repr(
# 36 "scish_parse.mly"
                        ( App(_2,_3) )
# 184 "scish_parse.ml"
               : Scish_ast.exp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Scish_ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Scish_ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Scish_ast.exp) in
    Obj.repr(
# 37 "scish_parse.mly"
                               ( If(_3,_4,_5) )
# 193 "scish_parse.ml"
               : Scish_ast.exp))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Scish_ast.exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Scish_ast.exp) in
    Obj.repr(
# 38 "scish_parse.mly"
                                             ( sLet _4 _5 _7 )
# 202 "scish_parse.ml"
               : Scish_ast.exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "scish_parse.mly"
  ( [] )
# 208 "scish_parse.ml"
               : Scish_ast.exp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Scish_ast.exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Scish_ast.exp list) in
    Obj.repr(
# 42 "scish_parse.mly"
           ( _1::_2 )
# 216 "scish_parse.ml"
               : Scish_ast.exp list))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "scish_parse.mly"
       ( Plus )
# 222 "scish_parse.ml"
               : Scish_ast.primop))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "scish_parse.mly"
        ( Minus )
# 228 "scish_parse.ml"
               : Scish_ast.primop))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "scish_parse.mly"
        ( Times )
# 234 "scish_parse.ml"
               : Scish_ast.primop))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "scish_parse.mly"
      ( Div )
# 240 "scish_parse.ml"
               : Scish_ast.primop))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "scish_parse.mly"
     ( Eq )
# 246 "scish_parse.ml"
               : Scish_ast.primop))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "scish_parse.mly"
     ( Lt )
# 252 "scish_parse.ml"
               : Scish_ast.primop))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "scish_parse.mly"
       ( Cons )
# 258 "scish_parse.ml"
               : Scish_ast.primop))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "scish_parse.mly"
      ( Fst )
# 264 "scish_parse.ml"
               : Scish_ast.primop))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "scish_parse.mly"
      ( Snd )
# 270 "scish_parse.ml"
               : Scish_ast.primop))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Scish_ast.exp)
