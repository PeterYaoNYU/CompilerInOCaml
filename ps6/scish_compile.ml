(* If you like, you can copy your solution from ps5 over here to then get
   a compiler from MLish -> Scish -> Cish. *)

exception Unimplemented
exception FatalError 

let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program = raise Unimplemented

