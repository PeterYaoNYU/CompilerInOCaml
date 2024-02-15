/* Parser for Fish --- TODO */

%{
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
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. Don't forget to add any new non-
 * terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt
%type <Ast.exp> exp
%type <Ast.binop> binop

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */
%token <int> INT 
%token <string> ID
%token PLUS MINUS TIMES DIV
%token EQ NEQ LT LTE GT GTE
%token NOT AND OR ASSIGN
%token IF ELSE WHILE FOR RETURN
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token EOF


%nonassoc LOWER_THAN_ASSIGNMENT
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIV
%left NOT

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

program:
  stmt EOF { $1 }

stmt:
    exp SEMICOLON { (Exp($1), rhs 1) }
  | LBRACE seq RBRACE { $2 }
  | IF LPAREN exp RPAREN stmt ELSE stmt { (If($3, $5, $7), rhs 1) }
  | WHILE LPAREN exp RPAREN stmt { (While($3, $5), rhs 1) }
  | FOR LPAREN exp SEMICOLON exp SEMICOLON exp RPAREN stmt { (For($3, $5, $7, $9), rhs 1) }
  | RETURN exp SEMICOLON { (Return($2), rhs 1) }
  | IF LPAREN exp RPAREN stmt { (If($3, $5, (Exp(Int 0,0), rhs 1)), rhs 1) }

seq:
    stmt { $1 }
  | stmt seq { (Seq($1, $2), rhs 2) }

exp: 
    INT                    { (Int($1), rhs 1) }
  | ID                     { (Var($1), rhs 1) }
  | exp PLUS exp           { (Binop($1, Plus, $3), rhs 2) }
  | exp MINUS exp          { (Binop($1, Minus, $3), rhs 2) }
  | MINUS exp              { (Binop((Int 0, rhs 1), Minus, $2), rhs 1) }
  | exp TIMES exp          { (Binop($1, Times, $3), rhs 2) }
  | exp DIV exp            { (Binop($1, Div, $3), rhs 2) }
  | exp EQ exp             { (Binop($1, Eq, $3), rhs 2) }
  | exp NEQ exp            { (Binop($1, Neq, $3), rhs 2) }
  | exp LT exp             { (Binop($1, Lt, $3), rhs 2) }
  | exp LTE exp            { (Binop($1, Lte, $3), rhs 2) }
  | exp GT exp             { (Binop($1, Gt, $3), rhs 2) }
  | exp GTE exp            { (Binop($1, Gte, $3), rhs 2) }
  | NOT exp                { (Not($2), rhs 1) }
  | exp AND exp            { (And($1, $3), rhs 2) }
  | exp OR exp             { (Or($1, $3), rhs 2) }
  | ID ASSIGN exp          { (Assign($1, $3), rhs 2) }
  | LPAREN exp RPAREN      { $2 }


binop:
    PLUS { Ast.Plus }
  | MINUS { Ast.Minus }
  | TIMES { Ast.Times }
  | DIV { Ast.Div }
  | EQ { Ast.Eq }
  | NEQ { Ast.Neq }
  | LT { Ast.Lt }
  | LTE { Ast.Lte }
  | GT { Ast.Gt }
  | GTE { Ast.Gte }

%%
