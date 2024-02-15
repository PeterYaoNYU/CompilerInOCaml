(* Register file definitions. A register file is a map from a register 
   number to a 32-bit quantity. *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
type regfile = int32 IntMap.t 
let empty_rf = IntMap.empty
let rf_update (r : int) (v : int32) (rf : regfile) : regfile = 
  IntMap.add r v rf
let rf_lookup (r : int) (rf : regfile) : int32 = 
  try IntMap.find r rf with Not_found -> Int32.zero
let string_of_rf (rf : regfile) : string = 
  IntMap.fold (fun key v s -> 
    s^(string_of_int key)^" -> "^(Int32.to_string v)^"\n") rf ""

open Int32
open Byte

type label = string

type reg = R0 
     | R1  (* DO NOT USE ME:  assembler temp *)
     | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 
     | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19
     | R20 | R21 | R22 | R23 | R24 | R25 
     | R26 | R27 (* DO NOT USE ME: reserved for OS *)
     | R28
     | R29 | R30 |R31  (* used for special purposes... *)

let reg2str r = 
  match r with
    R0 -> "$0" | R1 -> "$1" | R2 -> "$2" | R3 -> "$3"
  | R4 -> "$4" | R5 -> "$5" | R6 -> "$6" | R7 -> "$7"
  | R8 -> "$8" | R9 -> "$9" | R10 -> "$10" | R11 -> "$11"
  | R12 -> "$12" | R13 -> "$13" | R14 -> "$14" | R15 -> "$15"
  | R16 -> "$16" | R17 -> "$17" | R18 -> "$18" | R19 -> "$19"
  | R20 -> "$20" | R21 -> "$21" | R22 -> "$22" | R23 -> "$23"
  | R24 -> "$24" | R25 -> "$25" | R26 -> "$26" | R27 -> "$27"
  | R28 -> "$28" | R29 -> "$29" | R30 -> "$30" | R31 -> "$31"
  
let str2reg s =
  match s with
    "$0" -> R0 | "$1" -> R1 | "$2" -> R2 | "$3" -> R3 | "$4" -> R4
  | "$5" -> R5 | "$6" -> R6 | "$7" -> R7 | "$8" -> R8 | "$9" -> R9
  | "$10" -> R10 | "$11" -> R11 | "$12" -> R12 | "$13" -> R13
  | "$14" -> R14 | "$15" -> R15 | "$16" -> R16 | "$17" -> R17
  | "$18" -> R18 | "$19" -> R19 | "$20" -> R20 | "$21" -> R21
  | "$22" -> R22 | "$23" -> R23 | "$24" -> R24 | "$25" -> R25
  | "$26" -> R26 | "$27" -> R27 | "$28" -> R28 | "$29" -> R29
  | "$30" -> R30 | "$31" -> R31
  | _ -> R0

let reg2ind r =
  match r with
    R0 -> 0 | R1 -> 1 | R2 -> 2 | R3 -> 3
  | R4 -> 4 | R5 -> 5 | R6 -> 6 | R7 -> 7
  | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15
  | R16 -> 16 | R17 -> 17 | R18 -> 18 | R19 -> 19
  | R20 -> 20 | R21 -> 21 | R22 -> 22 | R23 -> 23
  | R24 -> 24 | R25 -> 25 | R26 -> 26 | R27 -> 27
  | R28 -> 28 | R29 -> 29 | R30 -> 30 | R31 -> 31
 
(* A small subset of the Mips assembly language *)
type inst =
  Add of reg * reg * reg  
| Beq of reg * reg * int32 (* should be int16, but ocaml doesn't have these *) 
| Jr of reg
| Jal of int32 
| Li of reg * int32        
| Lui of reg * int32       (* same here *)
| Ori of reg * reg * int32 (* and here ... *)
| Lw of reg * reg * int32  (* and here ... *)
| Sw of reg * reg * int32  (* and here ... *)

type program = inst list

(* Memory definitions. A memory is a map from 32-bit addresses to bytes. *)
module Int32Map = Map.Make(struct type t = int32 let compare = Int32.compare end)
type memory = byte Int32Map.t
let empty_mem = Int32Map.empty
let mem_update (a : int32) (v : byte) (m : memory) : memory =
  Int32Map.add a v m
let mem_lookup (a : int32) (m : memory) : byte =
  try (Int32Map.find a m) with Not_found -> mk_byte Int32.zero
let string_of_mem (m : memory) : string =
  Int32Map.fold (fun key v s ->
    s^(Int32.to_string key)^" -> "^(Int32.to_string (b2i32 v))^"\n") m ""

(* State *)
type state = { r : regfile; pc : int32; m : memory }

let reg_of_list (rl : (int*int32) list) : regfile =
  List.fold_right (fun (r,v) rf -> rf_update r v rf) rl empty_rf
;;
(*28: gp, 29: sp, 30: fp, 31: ra*)
let init_regfile = reg_of_list
[(28,0x30000000l);(29,0x7ffffffcl);(30,0x7ffffffcl)]
;;
let init_pc = 0x400000l
;;

let inst2str i =
  match i with
    Add (r1,r2,r3) -> "add "^(reg2str r1)^", "^(reg2str r2)^", "^(reg2str r3)^"\n"
  | Beq (r1,r2,imm) -> "beq "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Jr (r) -> "jr "^(reg2str r)^"\n"
  | Jal (imm) -> "jal "^(Int32.to_string imm)^"\n"
  | Li (r,imm) -> "li "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Lui (r,imm) -> "lui "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Ori (r1,r2,imm) -> "ori "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Lw (r1,r2,imm) -> "lw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"
  | Sw (r1,r2,imm) -> "sw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"

let rec prog2str p =
  match p with
    [] -> "\n"
  | hd::tl -> (inst2str hd)^(prog2str tl)
