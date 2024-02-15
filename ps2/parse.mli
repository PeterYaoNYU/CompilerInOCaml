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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
