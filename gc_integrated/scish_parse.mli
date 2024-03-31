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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Scish_ast.exp
