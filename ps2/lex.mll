(* Lexer for Fish --- TODO *)

(* You need to add new definition to build the
 * appropriate terminals to feed to parse.mly.
 *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

(* definition section *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter | digit | '_')*

(* rules section *)
(* Rules *)
rule lexer = parse
  | "/*" { comment lexbuf; lexer lexbuf }
  | eol { incr_lineno lexbuf; lexer lexbuf }
  | ws+ { lexer lexbuf }
  | digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "!" { NOT }
  | "&&" { AND }
  | "||" { OR }
  | "=" { ASSIGN }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "for" { FOR }
  | "return" { RETURN }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ";" { SEMICOLON }
  | identifier as id { ID id }
  | eof { EOF } 


and comment = parse
  | "*/"              { () }
  | '\n'              { incr_lineno lexbuf; comment lexbuf }
  | _                 { comment lexbuf }
  | eof               { failwith "Unclosed comment at end of file" }
