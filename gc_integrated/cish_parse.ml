type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EQEQ
  | NEQ
  | LTE
  | GTE
  | LT
  | GT
  | EQ
  | BANG
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | WHILE
  | FOR
  | LET
  | COMMA
  | MALLOC
  | INT of (int)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "cish_parse.mly"
open Cish_ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
# 47 "cish_parse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* EQEQ *);
  263 (* NEQ *);
  264 (* LTE *);
  265 (* GTE *);
  266 (* LT *);
  267 (* GT *);
  268 (* EQ *);
  269 (* BANG *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* TIMES *);
  273 (* DIV *);
  274 (* AND *);
  275 (* OR *);
  276 (* RETURN *);
  277 (* IF *);
  278 (* ELSE *);
  279 (* WHILE *);
  280 (* FOR *);
  281 (* LET *);
  282 (* COMMA *);
  283 (* MALLOC *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  284 (* INT *);
  285 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\005\000\005\000\004\000\004\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\007\000\007\000\018\000\018\000\008\000\008\000\008\000\
\009\000\009\000\010\000\010\000\011\000\011\000\012\000\012\000\
\012\000\013\000\013\000\013\000\013\000\013\000\014\000\014\000\
\014\000\015\000\015\000\015\000\016\000\016\000\016\000\016\000\
\016\000\017\000\017\000\017\000\017\000\017\000\017\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\005\000\002\000\003\000\001\000\003\000\
\001\000\002\000\003\000\003\000\007\000\005\000\005\000\009\000\
\006\000\001\000\002\000\000\000\001\000\001\000\003\000\004\000\
\001\000\003\000\001\000\003\000\001\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\003\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\001\000\002\000\002\000\002\000\
\002\000\001\000\001\000\003\000\004\000\004\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\056\000\001\000\000\000\000\000\000\000\
\003\000\005\000\000\000\000\000\000\000\000\000\006\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\050\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\042\000\
\000\000\008\000\000\000\000\000\000\000\051\000\048\000\046\000\
\047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\004\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\055\000\012\000\049\000\000\000\011\000\000\000\000\000\
\021\000\000\000\000\000\000\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\044\000\052\000\000\000\000\000\024\000\000\000\000\000\000\000\
\000\000\054\000\000\000\053\000\000\000\015\000\000\000\000\000\
\026\000\000\000\000\000\017\000\013\000\000\000\000\000\016\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\012\000\008\000\031\000\032\000\033\000\
\100\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\082\000"

let yysindex = "\008\000\
\026\255\000\000\046\255\000\000\000\000\026\255\012\255\053\255\
\000\000\000\000\043\255\072\255\087\255\050\255\000\000\000\000\
\015\255\087\255\036\255\036\255\036\255\036\255\015\255\081\255\
\102\255\107\255\067\255\119\255\000\000\117\255\087\255\123\255\
\135\255\118\255\121\255\019\255\003\255\031\255\051\255\000\000\
\139\255\000\000\147\255\152\255\036\255\000\000\000\000\000\000\
\000\000\146\255\159\255\015\255\015\255\015\255\150\255\015\255\
\015\255\000\000\000\000\000\000\036\255\036\255\036\255\036\255\
\036\255\036\255\036\255\036\255\036\255\036\255\036\255\036\255\
\005\255\000\000\000\000\000\000\015\255\000\000\167\255\168\255\
\000\000\177\255\015\255\176\255\000\000\121\255\019\255\003\255\
\003\255\031\255\031\255\031\255\031\255\051\255\051\255\000\000\
\000\000\000\000\155\255\180\255\000\000\087\255\087\255\015\255\
\190\255\000\000\015\255\000\000\170\255\000\000\194\255\087\255\
\000\000\087\255\015\255\000\000\000\000\193\255\087\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\199\000\000\000\000\000\
\000\000\000\000\197\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\116\255\199\255\000\000\
\000\000\021\255\073\255\082\000\063\000\242\255\179\255\000\000\
\137\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\158\255\000\000\000\000\000\000\201\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\094\255\101\000\073\000\
\087\000\007\000\021\000\035\000\049\000\200\255\221\255\000\000\
\000\000\000\000\209\255\000\000\000\000\000\000\000\000\201\255\
\000\000\000\000\000\000\000\000\057\255\000\000\000\000\000\000\
\000\000\000\000\210\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\210\000\000\000\000\000\203\000\000\000\240\000\248\255\239\255\
\113\000\000\000\160\000\161\000\030\000\189\000\036\000\238\255\
\000\000\157\255"

let yytablesize = 383
let yytable = "\043\000\
\047\000\048\000\049\000\050\000\111\000\051\000\017\000\098\000\
\001\000\044\000\065\000\066\000\067\000\068\000\010\000\118\000\
\017\000\019\000\020\000\021\000\022\000\022\000\058\000\022\000\
\063\000\064\000\076\000\019\000\020\000\021\000\022\000\028\000\
\029\000\030\000\079\000\080\000\081\000\017\000\084\000\085\000\
\011\000\028\000\029\000\030\000\069\000\070\000\022\000\007\000\
\019\000\020\000\021\000\045\000\096\000\097\000\003\000\099\000\
\013\000\014\000\014\000\101\000\014\000\014\000\028\000\029\000\
\046\000\105\000\071\000\072\000\014\000\014\000\014\000\014\000\
\014\000\027\000\015\000\027\000\014\000\014\000\011\000\014\000\
\014\000\014\000\052\000\014\000\014\000\014\000\081\000\016\000\
\017\000\099\000\018\000\027\000\088\000\089\000\028\000\055\000\
\028\000\081\000\027\000\019\000\020\000\021\000\022\000\053\000\
\094\000\095\000\023\000\024\000\054\000\025\000\026\000\027\000\
\028\000\028\000\029\000\030\000\051\000\051\000\051\000\028\000\
\056\000\051\000\051\000\051\000\051\000\051\000\051\000\059\000\
\057\000\051\000\051\000\051\000\051\000\051\000\051\000\060\000\
\061\000\045\000\062\000\045\000\073\000\051\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\074\000\045\000\045\000\
\045\000\045\000\045\000\045\000\075\000\077\000\049\000\078\000\
\049\000\083\000\045\000\049\000\049\000\049\000\049\000\049\000\
\049\000\102\000\103\000\049\000\049\000\049\000\049\000\049\000\
\049\000\104\000\106\000\039\000\107\000\039\000\108\000\049\000\
\039\000\039\000\039\000\039\000\039\000\039\000\112\000\114\000\
\039\000\039\000\115\000\119\000\039\000\039\000\002\000\007\000\
\040\000\020\000\040\000\018\000\039\000\040\000\040\000\040\000\
\040\000\040\000\040\000\025\000\020\000\040\000\040\000\009\000\
\042\000\040\000\040\000\113\000\086\000\041\000\087\000\041\000\
\000\000\040\000\041\000\041\000\041\000\041\000\041\000\041\000\
\000\000\000\000\041\000\041\000\000\000\000\000\041\000\041\000\
\000\000\000\000\034\000\000\000\034\000\000\000\041\000\034\000\
\034\000\034\000\034\000\034\000\034\000\090\000\091\000\092\000\
\093\000\000\000\000\000\034\000\034\000\000\000\000\000\037\000\
\000\000\037\000\000\000\034\000\037\000\037\000\037\000\037\000\
\037\000\037\000\000\000\000\000\000\000\038\000\000\000\038\000\
\037\000\037\000\038\000\038\000\038\000\038\000\038\000\038\000\
\037\000\000\000\000\000\035\000\000\000\035\000\038\000\038\000\
\035\000\035\000\035\000\035\000\035\000\035\000\038\000\000\000\
\000\000\036\000\000\000\036\000\035\000\035\000\036\000\036\000\
\036\000\036\000\036\000\036\000\035\000\000\000\000\000\031\000\
\000\000\031\000\036\000\036\000\031\000\031\000\000\000\000\000\
\000\000\032\000\036\000\032\000\000\000\000\000\032\000\032\000\
\031\000\031\000\029\000\000\000\029\000\109\000\110\000\033\000\
\031\000\033\000\032\000\032\000\033\000\033\000\000\000\116\000\
\000\000\117\000\032\000\029\000\029\000\030\000\120\000\030\000\
\033\000\033\000\000\000\029\000\000\000\000\000\000\000\000\000\
\033\000\000\000\000\000\000\000\000\000\000\000\030\000\030\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\000"

let yycheck = "\017\000\
\019\000\020\000\021\000\022\000\104\000\023\000\002\001\003\001\
\001\000\018\000\008\001\009\001\010\001\011\001\003\001\115\000\
\002\001\013\001\014\001\015\001\016\001\001\001\031\000\003\001\
\006\001\007\001\045\000\013\001\014\001\015\001\016\001\027\001\
\028\001\029\001\052\000\053\000\054\000\002\001\056\000\057\000\
\029\001\027\001\028\001\029\001\014\001\015\001\026\001\002\001\
\013\001\014\001\015\001\016\001\071\000\072\000\029\001\073\000\
\004\001\001\001\002\001\077\000\004\001\005\001\027\001\028\001\
\029\001\083\000\016\001\017\001\026\001\013\001\014\001\015\001\
\016\001\001\001\003\001\003\001\020\001\021\001\029\001\023\001\
\024\001\025\001\002\001\027\001\028\001\029\001\104\000\001\001\
\002\001\107\000\004\001\019\001\063\000\064\000\001\001\029\001\
\003\001\115\000\026\001\013\001\014\001\015\001\016\001\002\001\
\069\000\070\000\020\001\021\001\002\001\023\001\024\001\025\001\
\019\001\027\001\028\001\029\001\001\001\002\001\003\001\026\001\
\002\001\006\001\007\001\008\001\009\001\010\001\011\001\005\001\
\012\001\014\001\015\001\016\001\017\001\018\001\019\001\001\001\
\019\001\001\001\018\001\003\001\002\001\026\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\003\001\014\001\015\001\
\016\001\017\001\018\001\019\001\005\001\012\001\001\001\001\001\
\003\001\012\001\026\001\006\001\007\001\008\001\009\001\010\001\
\011\001\003\001\003\001\014\001\015\001\016\001\017\001\018\001\
\019\001\001\001\003\001\001\001\026\001\003\001\003\001\026\001\
\006\001\007\001\008\001\009\001\010\001\011\001\001\001\022\001\
\014\001\015\001\001\001\003\001\018\001\019\001\000\000\003\001\
\001\001\001\001\003\001\005\001\026\001\006\001\007\001\008\001\
\009\001\010\001\011\001\003\001\003\001\014\001\015\001\006\000\
\014\000\018\001\019\001\107\000\061\000\001\001\062\000\003\001\
\255\255\026\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\014\001\015\001\255\255\255\255\018\001\019\001\
\255\255\255\255\001\001\255\255\003\001\255\255\026\001\006\001\
\007\001\008\001\009\001\010\001\011\001\065\000\066\000\067\000\
\068\000\255\255\255\255\018\001\019\001\255\255\255\255\001\001\
\255\255\003\001\255\255\026\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\001\001\255\255\003\001\
\018\001\019\001\006\001\007\001\008\001\009\001\010\001\011\001\
\026\001\255\255\255\255\001\001\255\255\003\001\018\001\019\001\
\006\001\007\001\008\001\009\001\010\001\011\001\026\001\255\255\
\255\255\001\001\255\255\003\001\018\001\019\001\006\001\007\001\
\008\001\009\001\010\001\011\001\026\001\255\255\255\255\001\001\
\255\255\003\001\018\001\019\001\006\001\007\001\255\255\255\255\
\255\255\001\001\026\001\003\001\255\255\255\255\006\001\007\001\
\018\001\019\001\001\001\255\255\003\001\102\000\103\000\001\001\
\026\001\003\001\018\001\019\001\006\001\007\001\255\255\112\000\
\255\255\114\000\026\001\018\001\019\001\001\001\119\000\003\001\
\018\001\019\001\255\255\026\001\255\255\255\255\255\255\255\255\
\026\001\255\255\255\255\255\255\255\255\255\255\018\001\019\001\
\255\255\255\255\255\255\255\255\255\255\255\255\026\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  EQEQ\000\
  NEQ\000\
  LTE\000\
  GTE\000\
  LT\000\
  GT\000\
  EQ\000\
  BANG\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  LET\000\
  COMMA\000\
  MALLOC\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.func list) in
    Obj.repr(
# 50 "cish_parse.mly"
               ( _1 )
# 307 "cish_parse.ml"
               : Cish_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.func) in
    Obj.repr(
# 53 "cish_parse.mly"
       ( [_1] )
# 314 "cish_parse.ml"
               : Cish_ast.func list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.func) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.program) in
    Obj.repr(
# 54 "cish_parse.mly"
               ( _1::_2 )
# 322 "cish_parse.ml"
               : Cish_ast.func list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Cish_ast.var list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.stmt) in
    Obj.repr(
# 57 "cish_parse.mly"
                                    ( Fn{name=_1;args=_2;body=_4;pos=rhs 1} )
# 331 "cish_parse.ml"
               : Cish_ast.func))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "cish_parse.mly"
                ( [] )
# 337 "cish_parse.ml"
               : Cish_ast.var list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.var list) in
    Obj.repr(
# 61 "cish_parse.mly"
                       ( _2 )
# 344 "cish_parse.ml"
               : Cish_ast.var list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "cish_parse.mly"
     ( [_1] )
# 351 "cish_parse.ml"
               : Cish_ast.var list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.var list) in
    Obj.repr(
# 65 "cish_parse.mly"
                  ( _1::_3 )
# 359 "cish_parse.ml"
               : Cish_ast.var list))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "cish_parse.mly"
       ( (skip, rhs 1) )
# 365 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.exp) in
    Obj.repr(
# 69 "cish_parse.mly"
            ( (Exp _1, rhs 1) )
# 372 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.exp) in
    Obj.repr(
# 70 "cish_parse.mly"
                   ( (Return _2, rhs 1) )
# 379 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.stmt) in
    Obj.repr(
# 71 "cish_parse.mly"
                         ( _2 )
# 386 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Cish_ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.stmt) in
    Obj.repr(
# 72 "cish_parse.mly"
                                       ( (If(_3,_5,_7), rhs 1) )
# 395 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.stmt) in
    Obj.repr(
# 73 "cish_parse.mly"
                             ( (If(_3,_5,(skip, rhs 5)), rhs 1) )
# 403 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.stmt) in
    Obj.repr(
# 74 "cish_parse.mly"
                                ( (While(_3,_5), rhs 1) )
# 411 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : Cish_ast.exp option) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : Cish_ast.exp option) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp option) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.stmt) in
    Obj.repr(
# 75 "cish_parse.mly"
                                                        (
      let e1 = match _3 with None -> (Int(0), rhs 3) | Some e -> e in
      let e2 = match _5 with None -> (Int(1), rhs 5) | Some e -> e in
      let e3 = match _7 with None -> (Int(0), rhs 7) | Some e -> e in
      (For(e1,e2,e3,_9), rhs 1)
    )
# 426 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.stmt) in
    Obj.repr(
# 81 "cish_parse.mly"
                           ( (Let(_2,_4,_6), rhs 1) )
# 435 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.stmt) in
    Obj.repr(
# 84 "cish_parse.mly"
       ( _1 )
# 442 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.stmt) in
    Obj.repr(
# 85 "cish_parse.mly"
                ( (Seq(_1,_2), rhs 1) )
# 450 "cish_parse.ml"
               : Cish_ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "cish_parse.mly"
  ( None )
# 456 "cish_parse.ml"
               : Cish_ast.exp option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 89 "cish_parse.mly"
         ( Some _1 )
# 463 "cish_parse.ml"
               : Cish_ast.exp option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 92 "cish_parse.mly"
        ( _1 )
# 470 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 93 "cish_parse.mly"
             ( (Assign(_1,_3), rhs 1) )
# 478 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 94 "cish_parse.mly"
                         ( (Store(_2,_4), rhs 1) )
# 486 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 97 "cish_parse.mly"
       ( [_1] )
# 493 "cish_parse.ml"
               : Cish_ast.exp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp list) in
    Obj.repr(
# 98 "cish_parse.mly"
                     ( _1::_3 )
# 501 "cish_parse.ml"
               : Cish_ast.exp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 101 "cish_parse.mly"
         ( _1 )
# 508 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 102 "cish_parse.mly"
                  ( (Or(_1,_3), rhs 1) )
# 516 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 105 "cish_parse.mly"
           ( _1 )
# 523 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 106 "cish_parse.mly"
                      ( (And(_1,_3), rhs 1) )
# 531 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 109 "cish_parse.mly"
          ( _1 )
# 538 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 110 "cish_parse.mly"
                        ( (Binop(_1,Eq,_3), rhs 1) )
# 546 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 111 "cish_parse.mly"
                       ( (Binop(_1,Neq,_3), rhs 1) )
# 554 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 114 "cish_parse.mly"
         ( _1 )
# 561 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 115 "cish_parse.mly"
                    ( (Binop(_1,Lt,_3), rhs 1) )
# 569 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 116 "cish_parse.mly"
                    ( (Binop(_1,Gt,_3), rhs 1) )
# 577 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 117 "cish_parse.mly"
                     ( (Binop(_1,Lte,_3), rhs 1) )
# 585 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 118 "cish_parse.mly"
                     ( (Binop(_1,Gte,_3), rhs 1) )
# 593 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 121 "cish_parse.mly"
         ( _1 )
# 600 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 122 "cish_parse.mly"
                     ( (Binop(_1,Plus,_3), rhs 1) )
# 608 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 123 "cish_parse.mly"
                      ( (Binop(_1,Minus,_3), rhs 1) )
# 616 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 126 "cish_parse.mly"
           ( _1 )
# 623 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 127 "cish_parse.mly"
                        ( (Binop(_1,Times,_3), rhs 1) )
# 631 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 128 "cish_parse.mly"
                      ( (Binop(_1,Div,_3), rhs 1) )
# 639 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 131 "cish_parse.mly"
            ( _1 )
# 646 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 132 "cish_parse.mly"
                ( _2 )
# 653 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 133 "cish_parse.mly"
                 ( (Binop((Int 0,rhs 1),Minus,_2), rhs 1) )
# 660 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 134 "cish_parse.mly"
                ( (Not _2, rhs 1) )
# 667 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Cish_ast.exp) in
    Obj.repr(
# 135 "cish_parse.mly"
                 ( (Load _2, rhs 1) )
# 674 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 138 "cish_parse.mly"
      ( (Int _1, rhs 1) )
# 681 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 139 "cish_parse.mly"
     ( (Var _1, rhs 1) )
# 688 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Cish_ast.exp) in
    Obj.repr(
# 140 "cish_parse.mly"
                          ( (Call(_1,[]), rhs 1) )
# 695 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Cish_ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.exp list) in
    Obj.repr(
# 141 "cish_parse.mly"
                                  ( (Call(_1,_3), rhs 1) )
# 703 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.exp) in
    Obj.repr(
# 142 "cish_parse.mly"
                            ( (Malloc(_3), rhs 1) )
# 710 "cish_parse.ml"
               : Cish_ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Cish_ast.exp) in
    Obj.repr(
# 143 "cish_parse.mly"
                     ( _2 )
# 717 "cish_parse.ml"
               : Cish_ast.exp))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Cish_ast.program)
