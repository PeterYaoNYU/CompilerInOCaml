type token =
  | INT of (int)
  | ID of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | NOT
  | AND
  | OR
  | ASSIGN
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SEMICOLON
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
# 44 "parse.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* DIV *);
  263 (* EQ *);
  264 (* NEQ *);
  265 (* LT *);
  266 (* LTE *);
  267 (* GT *);
  268 (* GTE *);
  269 (* NOT *);
  270 (* AND *);
  271 (* OR *);
  272 (* ASSIGN *);
  273 (* IF *);
  274 (* ELSE *);
  275 (* WHILE *);
  276 (* FOR *);
  277 (* RETURN *);
  278 (* LPAREN *);
  279 (* RPAREN *);
  280 (* LBRACE *);
  281 (* RBRACE *);
  282 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\005\000\005\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\007\000\005\000\009\000\003\000\005\000\
\001\000\002\000\001\000\001\000\003\000\003\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\000\000\000\000\000\000\
\000\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\007\000\028\000\010\000\003\000\000\000\
\000\000\016\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\004\000\000\000\000\000\006\000"

let yydgoto = "\002\000\
\013\000\024\000\015\000\000\000\025\000"

let yysindex = "\011\000\
\178\255\000\000\000\000\000\255\004\255\004\255\247\254\255\254\
\021\255\004\255\004\255\178\255\000\000\018\000\069\255\004\255\
\253\254\000\000\004\255\004\255\004\255\082\255\200\255\178\255\
\029\255\000\000\004\255\004\255\004\255\004\255\004\255\004\255\
\004\255\004\255\004\255\004\255\004\255\004\255\000\000\082\000\
\221\255\242\255\095\255\000\000\000\000\000\000\000\000\253\254\
\253\254\000\000\000\000\117\000\117\000\024\255\024\255\024\255\
\024\255\107\000\095\000\178\255\178\255\004\255\039\255\000\000\
\108\255\178\255\004\255\000\000\024\000\178\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\041\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\121\255\000\000\000\000\000\000\000\000\000\000\000\000\033\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\237\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\142\255\
\163\255\000\000\000\000\170\255\249\255\034\000\036\000\038\000\
\058\000\053\000\252\254\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\004\000\000\000\037\000"

let yytablesize = 385
let yytable = "\014\000\
\008\000\029\000\030\000\027\000\003\000\004\000\027\000\005\000\
\017\000\018\000\026\000\001\000\019\000\022\000\023\000\016\000\
\006\000\026\000\026\000\040\000\020\000\026\000\041\000\042\000\
\043\000\011\000\027\000\028\000\029\000\030\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\021\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\047\000\012\000\012\000\
\066\000\009\000\063\000\064\000\046\000\000\000\000\000\012\000\
\068\000\065\000\012\000\000\000\071\000\000\000\069\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\000\000\037\000\038\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\039\000\037\000\
\038\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\044\000\037\000\038\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\062\000\037\000\038\000\015\000\015\000\000\000\000\000\015\000\
\015\000\015\000\015\000\015\000\015\000\067\000\015\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\013\000\013\000\015\000\000\000\013\000\013\000\013\000\013\000\
\013\000\013\000\000\000\013\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\014\000\014\000\013\000\
\000\000\014\000\014\000\014\000\014\000\014\000\014\000\000\000\
\014\000\014\000\003\000\004\000\000\000\005\000\000\000\018\000\
\018\000\014\000\000\000\000\000\014\000\000\000\006\000\000\000\
\018\000\000\000\007\000\018\000\008\000\009\000\010\000\011\000\
\000\000\012\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\000\000\037\000\038\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\045\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\000\000\037\000\038\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\060\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\000\000\037\000\
\038\000\008\000\008\000\000\000\008\000\000\000\019\000\019\000\
\061\000\000\000\000\000\000\000\000\000\008\000\000\000\019\000\
\000\000\008\000\019\000\008\000\008\000\008\000\008\000\000\000\
\008\000\008\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\000\000\037\000\038\000\000\000\
\020\000\020\000\021\000\021\000\022\000\022\000\070\000\020\000\
\020\000\021\000\021\000\022\000\022\000\000\000\000\000\000\000\
\020\000\000\000\021\000\020\000\022\000\021\000\000\000\022\000\
\023\000\023\000\025\000\025\000\000\000\000\000\000\000\023\000\
\023\000\000\000\000\000\025\000\000\000\000\000\025\000\000\000\
\023\000\000\000\000\000\023\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\000\000\037\000\
\038\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\000\000\037\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\027\000\
\028\000\029\000\030\000\000\000\000\000\033\000\034\000\035\000\
\036\000"

let yycheck = "\001\000\
\000\000\005\001\006\001\023\001\001\001\002\001\026\001\004\001\
\005\000\006\000\015\001\001\000\022\001\010\000\011\000\016\001\
\013\001\000\000\023\001\016\000\022\001\026\001\019\000\020\000\
\021\000\022\001\003\001\004\001\005\001\006\001\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\022\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\025\001\014\001\015\001\
\018\001\025\001\060\000\061\000\024\000\255\255\255\255\023\001\
\066\000\062\000\026\001\255\255\070\000\255\255\067\000\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\255\255\014\001\015\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\026\001\014\001\
\015\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\026\001\014\001\015\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\026\001\014\001\015\001\003\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\026\001\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\023\001\
\003\001\004\001\026\001\255\255\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\023\001\003\001\004\001\026\001\
\255\255\007\001\008\001\009\001\010\001\011\001\012\001\255\255\
\014\001\015\001\001\001\002\001\255\255\004\001\255\255\014\001\
\015\001\023\001\255\255\255\255\026\001\255\255\013\001\255\255\
\023\001\255\255\017\001\026\001\019\001\020\001\021\001\022\001\
\255\255\024\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\023\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\255\255\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\023\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\001\001\002\001\255\255\004\001\255\255\014\001\015\001\
\023\001\255\255\255\255\255\255\255\255\013\001\255\255\023\001\
\255\255\017\001\026\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\255\255\
\007\001\008\001\007\001\008\001\007\001\008\001\023\001\014\001\
\015\001\014\001\015\001\014\001\015\001\255\255\255\255\255\255\
\023\001\255\255\023\001\026\001\023\001\026\001\255\255\026\001\
\007\001\008\001\014\001\015\001\255\255\255\255\255\255\014\001\
\015\001\255\255\255\255\023\001\255\255\255\255\026\001\255\255\
\023\001\255\255\255\255\026\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\003\001\
\004\001\005\001\006\001\255\255\255\255\009\001\010\001\011\001\
\012\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LTE\000\
  GT\000\
  GTE\000\
  NOT\000\
  AND\000\
  OR\000\
  ASSIGN\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    Obj.repr(
# 60 "parse.mly"
           ( _1 )
# 274 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 63 "parse.mly"
                  ( (Exp(_1), rhs 1) )
# 281 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'seq) in
    Obj.repr(
# 64 "parse.mly"
                      ( _2 )
# 288 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 65 "parse.mly"
                                        ( (If(_3, _5, _7), rhs 1) )
# 297 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 66 "parse.mly"
                                 ( (While(_3, _5), rhs 1) )
# 305 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : Ast.exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 67 "parse.mly"
                                                           ( (For(_3, _5, _7, _9), rhs 1) )
# 315 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 68 "parse.mly"
                         ( (Return(_2), rhs 1) )
# 322 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 69 "parse.mly"
                              ( (If(_3, _5, (Exp(Int 0,0), rhs 1)), rhs 1) )
# 330 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 72 "parse.mly"
         ( _1 )
# 337 "parse.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 73 "parse.mly"
             ( (Seq(_1, _2), rhs 2) )
# 345 "parse.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 76 "parse.mly"
                           ( (Int(_1), rhs 1) )
# 352 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parse.mly"
                           ( (Var(_1), rhs 1) )
# 359 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 78 "parse.mly"
                           ( (Binop(_1, Plus, _3), rhs 2) )
# 367 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 79 "parse.mly"
                           ( (Binop(_1, Minus, _3), rhs 2) )
# 375 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 80 "parse.mly"
                           ( (Binop((Int 0, rhs 1), Minus, _2), rhs 1) )
# 382 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 81 "parse.mly"
                           ( (Binop(_1, Times, _3), rhs 2) )
# 390 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 82 "parse.mly"
                           ( (Binop(_1, Div, _3), rhs 2) )
# 398 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 83 "parse.mly"
                           ( (Binop(_1, Eq, _3), rhs 2) )
# 406 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 84 "parse.mly"
                           ( (Binop(_1, Neq, _3), rhs 2) )
# 414 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 85 "parse.mly"
                           ( (Binop(_1, Lt, _3), rhs 2) )
# 422 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 86 "parse.mly"
                           ( (Binop(_1, Lte, _3), rhs 2) )
# 430 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 87 "parse.mly"
                           ( (Binop(_1, Gt, _3), rhs 2) )
# 438 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 88 "parse.mly"
                           ( (Binop(_1, Gte, _3), rhs 2) )
# 446 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 89 "parse.mly"
                           ( (Not(_2), rhs 1) )
# 453 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 90 "parse.mly"
                           ( (And(_1, _3), rhs 2) )
# 461 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 91 "parse.mly"
                           ( (Or(_1, _3), rhs 2) )
# 469 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 92 "parse.mly"
                           ( (Assign(_1, _3), rhs 2) )
# 477 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 93 "parse.mly"
                           ( _2 )
# 484 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parse.mly"
         ( Ast.Plus )
# 490 "parse.ml"
               : Ast.binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parse.mly"
          ( Ast.Minus )
# 496 "parse.ml"
               : Ast.binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parse.mly"
          ( Ast.Times )
# 502 "parse.ml"
               : Ast.binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parse.mly"
        ( Ast.Div )
# 508 "parse.ml"
               : Ast.binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parse.mly"
       ( Ast.Eq )
# 514 "parse.ml"
               : Ast.binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parse.mly"
        ( Ast.Neq )
# 520 "parse.ml"
               : Ast.binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parse.mly"
       ( Ast.Lt )
# 526 "parse.ml"
               : Ast.binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parse.mly"
        ( Ast.Lte )
# 532 "parse.ml"
               : Ast.binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parse.mly"
       ( Ast.Gt )
# 538 "parse.ml"
               : Ast.binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parse.mly"
        ( Ast.Gte )
# 544 "parse.ml"
               : Ast.binop))
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
;;
