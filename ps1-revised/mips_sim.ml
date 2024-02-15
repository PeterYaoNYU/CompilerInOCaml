open Mips_ast
open Mips_assem
open Byte

exception TODO
exception FatalError



(* Helper function to get the next instruction from memory *)
let load_word (pc:int32) (mem:memory) : int32 =
  let byte_0 = mem_lookup pc mem in 
  let byte_1 = mem_lookup (Int32.add pc 1l) mem in
  let byte_2 = mem_lookup (Int32.add pc 2l) mem in
  let byte_3 = mem_lookup (Int32.add pc 3l) mem in
  combine_bits [( b2i32 byte_0, 8); (b2i32 byte_1, 8); (b2i32 byte_2, 8); (b2i32 byte_3, 8)]
;;

let decode_word (w : int32) : inst = 
  let opcode = Int32.shift_right_logical w 26 in
  let rs = Int32.shift_right_logical (Int32.shift_left w 6) 27 in 
  let rt = Int32.shift_right_logical (Int32.shift_left w 11) 27 in
  let rd = Int32.shift_right_logical (Int32.shift_left w 16) 27 in
  (* let r_shamt = Int32.shift_right_logical  (Int32.shift_left w 6) 27 in *)
  let funct = Int32.logand w 0x3fl in
  let immediate = Int32.logand w 0xffffl in
  Printf.printf "finish extracting\n";
  Printf.printf "Opcode: %lx, rs: %lx, rt: %lx, rd: %lx, funct: %lx, immediate: %lx\n" opcode rs rt rd funct immediate;
  match opcode with
  | 0l -> 
    (
      match funct with 
      | 0x20l -> Add (ind2reg rd, ind2reg rs, ind2reg rt)  (* Add *)
      | 0x08l -> Jr (ind2reg rs)  (* Jr, note rs is used *)
      | _ -> Printf.printf "Unknown funct: %lx\n" funct; raise FatalError
    )
  | 0x03l -> Jal (immediate)  (* Jal *)
  | 0x04l -> Beq (ind2reg rs, ind2reg rt, immediate)  (* Beq *)
  | 0x0Fl -> Lui (ind2reg rt, immediate)  (* Lui *)
  | 0x0Dl -> Ori (ind2reg rt, ind2reg rs, immediate)  (* Ori *)
  | 0x23l -> Lw (ind2reg rt, ind2reg rs, immediate)  (* Lw *)
  | 0x2Bl -> Sw (ind2reg rt, ind2reg rs, immediate)  (* Sw *)
  | _ -> Printf.printf "Unknown opcode: %lx\n" opcode; raise FatalError
;;



let step_add (rd: reg) (rs: reg) (rt: reg) (state: state) : state =
  Printf.printf "adding\n";
  let rs_val = rf_lookup (reg2ind rs) state.r in 
  let rt_val = rf_lookup (reg2ind rt) state.r in 
  let sum = Int32.add rs_val rt_val in 
  Printf.printf "step_add: $%s + $%s -> $%s (%lx + %lx -> %lx)\n"
  (reg2str rs) (reg2str rt) (reg2str rd) rs_val rt_val sum;
  { state with r = rf_update (reg2ind rd) sum state.r }
;;

let step_jr (rs: reg) (state: state) : state = 
  let rs_val = rf_lookup (reg2ind rs) state.r in 
  { state with pc = rs_val }
;;


(* remember that in jal, we need to shift the immediate value left by 2 before adding to the pc *)
(* while in step jr, this step is not necessary *)
(* it should also concat the four most significant bits *)
let step_jal (immediate:int32) (state : state) : state = 
  let return_address = Int32.add state.pc 4l in
  let reg_with_ra = rf_update 31 return_address state.r in
  let target_address = Int32.add (Int32.shift_left immediate 2) (Int32.logand (Int32.shift_right_logical state.pc 28) 0xF0000000l) in
  {state with
    r = reg_with_ra;
    pc = target_address
  }
;;

let step_beq (rs: reg) (rt: reg) (immediate: int32) (state: state) : state = 
  let rs_val = rf_lookup (reg2ind rs) state.r in 
  let rt_val = rf_lookup (reg2ind rt) state.r in 
  if rs_val = rt_val then
    let adjusted_pc = Int32.sub state.pc 4l in 
    let target_offset = Int32.shift_left immediate 2 in 
    { state with pc = Int32.add adjusted_pc target_offset }
  else
    state
;;


let step_lui (rt: reg) (immediate: int32) (state: state) : state =
  let rt_val = Int32.shift_left immediate 16 in 
  { state with r = rf_update (reg2ind rt) rt_val state.r }  
;;

let step_ori (rt: reg) (rs: reg) (immediate: int32) (state: state) : state =
  let rs_val = rf_lookup (reg2ind rs) state.r in 
  let rt_val = Int32.logor rs_val immediate in 
  { state with r = rf_update (reg2ind rt) rt_val state.r }
;;

let step_lw (rt: reg) (rs: reg) (immediate: int32) (state: state) : state =
  let rs_val = rf_lookup (reg2ind rs) state.r in 
  let mem_val = mem_lookup (Int32.add rs_val immediate) state.m in 
  { state with r = rf_update (reg2ind rt) (b2i32 mem_val) state.r }
;;

let step_sw (rt: reg) (rs: reg) (immediate: int32) (state: state) : state =
  let rs_val = rf_lookup (reg2ind rs) state.r in 
  let rt_val = rf_lookup (reg2ind rt) state.r in 
  let new_mem = mem_update (Int32.add rs_val immediate) (mk_byte rt_val) state.m in 
  { state with m = new_mem }
;;



(* Take a look at the definition of the Mips AST and machine state in mips_ast.ml *)

(* Given a starting state, simulate the Mips machine code to get a final state;
   a final state is reached if the the next instruction pointed to by the PC is
   all 0s.
 *)
 let rec interp (state : state) : state = 

  Printf.printf "Current PC: %lx\n" state.pc;  (* Print the current PC *)
  (* Increment the PC first *)
  let updated_state = { state with pc = Int32.add state.pc 4l } in

  (* Load the word from the original PC, not the incremented one *)
  let word = load_word state.pc state.m in
  Printf.printf "Executing instruction: %lx\n" word;  (* Print the instruction word *)

  if word = 0l then
    begin
      Printf.printf "Reached end of program.\n";
      state
    end
  else
    let next_state = 
      match decode_word word with
      | Add (rd, rs, rt) -> step_add rd rs rt updated_state
      | Jr rs -> step_jr rs updated_state
      | Jal immediate -> step_jal immediate updated_state
      | Beq (rs, rt, immediate) -> step_beq rs rt immediate updated_state
      | Lui (rt, immediate) -> step_lui rt immediate updated_state
      | Ori (rt, rs, immediate) -> step_ori rt rs immediate updated_state
      | Lw (rt, rs, immediate) -> step_lw rt rs immediate updated_state
      | Sw (rt, rs, immediate) -> step_sw rt rs immediate updated_state
      | _ -> raise FatalError
    in
    interp next_state  (* Continue with the updated state *)




(*
  Here are a few details/assumptions about the assembler and interpreter that the autograder makes:
  * > Big Endian Encoding
  * > Program Data is stored starting at 0x400000
  * > Stack grows downward starting at 0x7ffffffc
  * > GP points to 30000000
  * > The assembler uses register 1 as temp storage for encoding Li
  * > We don't implement delay slots in either assembly or bitcode semantics
  * > As stated in lecture, we shift jump and break immediates left by 2
  * > The PC is always incremented before executing an instruction
  * > Beq subtracts 4 from the PC before adding its offset
  * > We preserve the top 4 bits of the PC when executing a jal
*)
