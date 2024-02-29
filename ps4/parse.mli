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
  | INT of (int)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
