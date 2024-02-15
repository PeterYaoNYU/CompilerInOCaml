(* file: main.ml
   author: Robert Muller, J. Tassarotti

  Problem set 0: exercises for coming up to speed in OCaml.

  Usage from parent directory:

   > dune exec bin/main.exe test

   Note that omitting "test" in the above won't run the tests.

   There are 10 problems worth 6 points total.

   1. isDigit (.25)
   2. newFileName (.25)
   3. homePath (.25)
   4. atoi (.25)
   5. tokenize (1)
   6. formatTree (1)
   7. subpalindrome (.75)
   8. listComparisons (.5)
   9. patience (1)
   10. FilteredSet module (.75)
*)

(* Problem 1: .25 points

   isDigit : char -> bool
*)

let isDigit (a:char) : bool = 
  match a with 
  | '0' -> true
  | '1' -> true
  | '2' -> true
  | '3' -> true
  | '4' -> true
  | '5' -> true
  | '6' -> true
  | '7' -> true
  | '8' -> true
  | '9' -> true
  | _ -> false

(* Problem 2: .25 points

   newFileName : string -> string -> string
*)
let newFileName (name:string) (ext:string) : string =
  let idx : (int option) = String.rindex_opt name '.' in
  match idx with
  | Some x -> 
    let left = String.sub name 0 x in
    String.concat "" [left; "."; ext]
  | None -> name

(* Problem 3: .25 points

   homePath : unit -> string list
*)
let homePath () = 
  let home = Unix.getenv "HOME" in
  let split = String.split_on_char '/' home in
  split

(* Problem 4: .25 points

   atoi : string -> int
*)
 (* let atoi (s:string) : int = int_of_string s *)

 let atoi (s:string) : int = 
  let exploded = Lib.explode s in
  let rec helper (l:char list) (acc:int) : int =
    match l with
    | [] -> acc
    | x :: xs -> 
      let num = (int_of_char x) - (int_of_char '0') in
      let new_acc = acc * 10 + num in
      helper xs new_acc
  in
  helper exploded 0


type token = If | And | Or

(* Problem 5: 1 points

   tokenize : string -> token list
*)
let tokenize (s:string) = 
  let exploded = Lib.explode s in
  let rec helper (l:char list) (acc:token list) : token list =
    match l with
    | [] -> acc
    | x :: xs -> 
      match x with
      | ' ' -> helper xs acc
      | '|' -> 
        let next = List.hd xs in
        if next = '|' then helper (List.tl xs) (acc @ [Or])
        else helper xs acc
      | '&' -> 
        let next = List.hd xs in
        if next = '&' then helper (List.tl xs) (acc @ [And])
        else helper xs acc
      | 'i' -> 
        let next = List.hd xs in
        if next = 'f' then helper (List.tl xs) (acc @ [If])
        else helper xs acc
      | _ -> helper xs acc
  in
  helper exploded []


(* Two problems related to trees. We have arrows as interior nodes
   and leaves that include a unique constant C and variables
   {v1, v2, ... }

               ->             ->
              /  \           /  \
            v0   ->         C    v1
                /  \
               C    C
 *)

(* Note that the 'too' field should probably be called 'to' (that is the names would be from and to), but 'to'
   is already a reserved keyword in OCaml, so we cannot use it for a field name. *)

type t = C
       | Var of int
       | Arrow of { from : t
                  ; too  : t
                  }

(* Problem 6: 1 point

   formatTree : t -> string
*)
let formatTree (tree : t) : string =
  let rec helper (t:t) (acc:string) : string =
    match t with
    | C -> acc ^ "C"
    | Var x -> acc ^ "v" ^ (string_of_int x)
    | Arrow {from; too} -> 
      let left = helper from acc in
      let right = helper too acc in
      "(" ^ left ^ " -> " ^ right ^ ")"
  in
  helper tree ""

(* Problem 7: .75 point 

   subplaindrome : string -> string
*)
let subpalindrome (s : string) : string = 
  let rec helper (s : string) (acc : string) : string =
    match s with
    | "" -> acc
    | _ -> 
      let len = String.length s in
      let first = String.sub s 0 1 in
      let last = String.sub s (len - 1) 1 in
      if String.length s = 1 then acc 
      else if first = last then helper (String.sub s 1 (len - 2)) acc
      else helper (String.sub s 1 (len - 2)) (String.sub s 1 (len - 2))
  in
  helper s s

(* Problem 8: .5 point

   list_comparisons : int list -> comparison list
*)
type comparison = GEQ | LT

let listComparisons (l : int list) : comparison list =
  let helper (l : int list) (idx : int) : comparison =
    match idx with
    | 0 -> GEQ
    | _ -> 
      let prev = List.nth l (idx - 1) in
      let curr = List.nth l idx in
      if curr >= prev then GEQ
      else LT
  in List.map (helper l) (Lib.range (List.length l))
  

(* Problem 9: 1 point

   patience : (int list) list -> int -> (int list) list
*)
let patience (ls : (int list) list) (num : int) : (int list) list =
  let rec helper (ls : (int list) list) (acc : (int list) list) (keepCmp : bool)=
    match ls with
    | [] -> if keepCmp then acc @ [[num]] else acc
    | x :: xs -> 
      let hd_num = List.hd x in
      if keepCmp && (num <= hd_num) then helper xs (acc @ [num :: x]) false
      else helper xs (acc @ [x]) keepCmp
  in
  helper ls [] true

(* Problem 10 : .75 points *)

module type FilteredSetType = sig
  type t
  val newSet : (int -> bool) -> t
  val insert : int -> t -> t
  val member : int -> t -> bool
  val mapAndFilter : (int -> int) -> t -> t
end

module FilteredSet : FilteredSetType =
struct
  type t = {filter : (int -> bool); set : int list}
  let newSet f = {filter = f; set = []}
  let insert num filteredSet = 
    if (filteredSet.filter num) then {filter = filteredSet.filter; set = num :: filteredSet.set}
    else filteredSet
  let member num filteredSet = List.mem num filteredSet.set
  let mapAndFilter f filteredSet = 
    let mapped = List.map f filteredSet.set in
    let filtered = List.filter filteredSet.filter mapped in
    {filter = filteredSet.filter; set = filtered}
end


(* TESTING **********************************************************)

type parts = One       (* isDigit *)
           | Two       (* newFileName *)
           | Three     (* homePath *)
           | Four      (* atoi *)
           | Five      (* tokenize *)
           | Six       (* formatTree *)
           | Seven       (* subpalindrome *)
           | Eight       (* listComparisons *)
           | Nine       (* patience *)
           | Ten       (* FilteredSet module *)

(* Some simple test data
*)
let v0 = Lib.fresh()
let v1 = Lib.fresh()
let v2 = Lib.fresh()

(*            t0 = ->        t1 = ->
                  /  \           /  \
                v0   ->         C    v1
                    /  \
                   C    C
*)
let t0 = Arrow { from = Var v0
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }

let t1 = Arrow { from = C
               ; too  = Var v1
               }

let t2 = Arrow { from = t1
               ; too  = Arrow { from = C
                              ; too  = C
                              }
               }
let t3 = Arrow { from = C
               ; too  = t0
               }

(********************************************************************)

(* Test isDigit
*)
let isDigitTest1 () = isDigit '0'
let isDigitTest2 () = isDigit '9'
let isDigitTest3 () = not (isDigit 'A')
let isDigitTests () =
  Lib.run_test "isDigit test1" isDigitTest1 ;
  Lib.run_test "isDigit test2" isDigitTest2 ;
  Lib.run_test "isDigit test3" isDigitTest3

(* Test newFileName
*)
let newFileNameTest1 () = newFileName "A.java" "class" = "A.class"
let newFileNameTest2 () = newFileName "A" "class" = "A"
let newFileNameTest3 () = newFileName "A.B.java" "class" = "A.B.class"
let newFileNameTest4 () = newFileName "A." "class" = "A.class"
let newFileNameTests () =
  Lib.run_test "newFileName test1" newFileNameTest1 ;
  Lib.run_test "newFileName test2" newFileNameTest2 ;
  Lib.run_test "newFileName test3" newFileNameTest3 ;
  Lib.run_test "newFileName test4" newFileNameTest4

(* Test homePath
*)
let homePathTest () =
  let answer = homePath () in
  let combiner name1 name2 = Lib.fmt "%s/%s" name1 name2 in
  let path = List.fold_left combiner "" answer
  in
  try
    Unix.chdir path = ()
  with
    Unix.Unix_error _ -> failwith "homeTest failed"
let homePathTests () = Lib.run_test "home test" homePathTest

(* Test atoi
*)
let atoiTest1 () = atoi "345" = 345
let atoiTest2 () = atoi "0" = 0
let atoiTests () =
  Lib.run_test "atoi test1" atoiTest1 ;
  Lib.run_test "atoi test2" atoiTest2

(* Test tokenize
*)
let tokenizeTest1 () =
  (tokenize "|| if && if && ") = [Or; If; And; If; And]
let tokenizeTest2 () = (tokenize "||") = [Or]
let tokenizeTest3 () = (tokenize "       ") = []
let tokenizeTests () =
  Lib.run_test "tokenize test1" tokenizeTest1 ;
  Lib.run_test "tokenize test2" tokenizeTest2 ;
  Lib.run_test "tokenize test3" tokenizeTest3

(* Test formatTree
*)
let formatTreeTest1 () = (formatTree t0) = "(v0 -> (C -> C))"
let formatTreeTest2 () = (formatTree t1) = "(C -> v1)"
let formatTreeTest3 () = (formatTree (Var v2)) = "v2"
let formatTreeTests () =
  Lib.run_test "formatTree test1" formatTreeTest1 ;
  Lib.run_test "formatTree test2" formatTreeTest2 ;
  Lib.run_test "formatTree test3" formatTreeTest3

(* Test subpalindrome
*)
let subpalindromeTest1 () = subpalindrome "aba" = "aba"
let subpalindromeTest2 () = subpalindrome "dabac" = "aba"
let subpalindromeTest3 () = subpalindrome "xx" = "xx"
let subpalindromeTest4 () = subpalindrome "x1amanaplanacanalpanamax1" = "amanaplanacanalpanama"
let subpalindromeTest5 () = subpalindrome "civic" = "civic"
let subpalindromeTest6 () = subpalindrome "deified" = "deified"
let subpalindromeTest7 () = subpalindrome "2eifie+" = "eifie"
let subpalindromeTest8 () = subpalindrome "xyz" = "y"
let subpalindromeTest9 () = subpalindrome "" = ""
let subpalindromeTests () =
  Lib.run_test "subpalindrome test1" subpalindromeTest1 ;
  Lib.run_test "subpalindrome test2" subpalindromeTest2 ;
  Lib.run_test "subpalindrome test3" subpalindromeTest3 ;
  Lib.run_test "subpalindrome test4" subpalindromeTest4 ;
  Lib.run_test "subpalindrome test5" subpalindromeTest5 ;
  Lib.run_test "subpalindrome test6" subpalindromeTest6 ;
  Lib.run_test "subpalindrome test7" subpalindromeTest7 ;
  Lib.run_test "subpalindrome test8" subpalindromeTest8 ;
  Lib.run_test "subpalindrome test9" subpalindromeTest9

let listComparisonsTest1 () = listComparisons [3] = [GEQ]
let listComparisonsTest2 () = listComparisons [3;4;5] = [GEQ; GEQ; GEQ]
let listComparisonsTest3 () = listComparisons [1;-1;1] = [GEQ; LT; GEQ]
let listComparisonsTest4 () = listComparisons [-1;-1;1] = [GEQ; GEQ; GEQ]
let listComparisonsTest5 () = listComparisons [9;8;7] = [GEQ; LT; LT]
let listComparisonsTest6 () = listComparisons [9;8;7;10] = [GEQ; LT; LT; GEQ]
let listComparisonsTests () =
  Lib.run_test "listComparisons test1" listComparisonsTest1 ;
  Lib.run_test "listComparisons test2" listComparisonsTest2 ;
  Lib.run_test "listComparisons test3" listComparisonsTest3 ;
  Lib.run_test "listComparisons test4" listComparisonsTest4 ;
  Lib.run_test "listComparisons test5" listComparisonsTest5 ;
  Lib.run_test "listComparisons test6" listComparisonsTest6 

let patienceTest1 () = patience [[3]] 4 = [[3]; [4]]
let patienceTest2 () = patience [] 3 = [[3]]
let patienceTest3 () = patience [[4]; [5]] 3 = [[3;4]; [5]]
let patienceTest4 () = patience [[2]; [6]] 4 = [[2]; [4;6]]
let patienceTest5 () = patience [[2]; [6]; [10]] 8 = [[2]; [6]; [8; 10]]
let patienceTest6 () = patience [[2]; [6]; [10]] 12 = [[2]; [6]; [10]; [12]]
let patienceTest7 () = patience [[2]; [3;6]; [10]] 3 = [[2]; [3;3;6]; [10]]
let patienceTest8 () = patience [[2]; [3]; [4]; [5]; [6]] 4 = [[2]; [3]; [4;4]; [5]; [6]]
let patienceTests () =
  Lib.run_test "patience test1" patienceTest1 ;
  Lib.run_test "patience test2" patienceTest2 ;
  Lib.run_test "patience test3" patienceTest3 ;
  Lib.run_test "patience test4" patienceTest4 ;
  Lib.run_test "patience test5" patienceTest5 ;
  Lib.run_test "patience test6" patienceTest6 ;
  Lib.run_test "patience test7" patienceTest7 ;
  Lib.run_test "patience test8" patienceTest8


let isEven n = (n mod 2 = 0)

open FilteredSet

let rec insert_list xs s =
  match xs with
  | [] -> s
  | x :: xs -> insert_list xs (insert x s)

let filteredSetTests_wrapper () =
  let evenSet_empty = newSet isEven in

  let evenSet_1 = insert_list [1;2;3;4;5;6] evenSet_empty in
  let evenSet_2 = insert_list [10;12;13] evenSet_empty in

  let lt5Set_empty = newSet ((>) 5) in

  let lt5Set_1 = insert_list [1;2;3;4;5;6] lt5Set_empty in
  let lt5Set_2 = mapAndFilter ((+) 2) lt5Set_1 in
  let filteredSetTest1 () = member 5 evenSet_1 = false in
  let filteredSetTest2 () = member 2 evenSet_1 = true in
  let filteredSetTest3 () = member 2 evenSet_2 = false in
  let filteredSetTest4 () = member 12 evenSet_2 = true in
  let filteredSetTest5 () = member 4 lt5Set_1 = true in
  let filteredSetTest6 () = member 5 lt5Set_1 = false in
  let filteredSetTest7 () = member 6 lt5Set_2 = false in
  let filteredSetTest8 () = member 4 lt5Set_2 = true in
  let filteredSetTest9 () = member 1 lt5Set_2 = false in
  Lib.run_test "filteredSet test1" filteredSetTest1 ;
  Lib.run_test "filteredSet test2" filteredSetTest2 ;
  Lib.run_test "filteredSet test3" filteredSetTest3 ;
  Lib.run_test "filteredSet test4" filteredSetTest4 ;
  Lib.run_test "filteredSet test5" filteredSetTest5 ;
  Lib.run_test "filteredSet test6" filteredSetTest6 ;
  Lib.run_test "filteredSet test7" filteredSetTest7 ;
  Lib.run_test "filteredSet test8" filteredSetTest8 ;
  Lib.run_test "filteredSet test9" filteredSetTest9

let filteredSetTests () =
  try filteredSetTests_wrapper () with
  | Failure s -> print_endline ("filteredSet tests error: `" ^ s ^ "`\n")
  | e -> print_endline ("filteredSet tests error: `" ^ Printexc.to_string e ^ "`\n")

(******************************************************************)

(******************************************************************)

let test part =
  match part with
  | One   -> isDigitTests()
  | Two   -> newFileNameTests()
  | Three -> homePathTests()
  | Four  -> atoiTests()
  | Five  -> tokenizeTests()
  | Six   -> formatTreeTests()
  | Seven   -> subpalindromeTests ()
  | Eight   -> listComparisonsTests ()
  | Nine -> patienceTests ()
  | Ten  -> filteredSetTests ()

let run () =
  let () = test One in
  let () = test Two in
  let () = test Three in
  let () = test Four in
  let () = test Five in
  let () = test Six in
  let () = test Seven in
  let () = test Eight in
  let () = test Nine in
  let () = test Ten in
  ()

let () =
  if (Array.length Sys.argv = 2 && Sys.argv.(1) = "test") then
    run ()
  else
    ()
