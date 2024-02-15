open Mips_ast
open Byte

exception TODO
exception FatalError
exception PError of string

(*instformat is an intermediate format between ast instructions and bytes*)
type rformat_args =  {r_opcode : int32; r_rs: int32; r_rt: int32; r_rd: int32; 
  r_shamt: int32; r_fun: int32} 
type iformat_args = {i_opcode : int32; i_rs: int32; i_rt: int32; i_imm: int32}
type jformat_args =  {j_opcode : int32; j_addr: int32}
type instformat = R of rformat_args| I of iformat_args| J of jformat_args
;;

(*00001111111 with n ones*)
let rec ones n : int32 = 
  if (n=0) then Int32.zero else
  Int32.add
    (Int32.shift_left Int32.one (n-1))
    (ones (n-1))
;;

(*rewrite all pseudo instructions*)
let rec rem_pseudo (prog : program) : program = List.fold_right
  (fun (i:inst) (p:program) ->
    (match i with 
    | Li (rd, imm) -> 
      (Lui (R1,Int32.shift_right_logical imm 16)::
        (Ori (rd,R1,Int32.logand imm (ones 16))::p)
      )
    | x -> x::p
    )
  )
  prog []
;;

let reg2ind32 r = Int32.of_int (reg2ind r)
;;

(*AST instruction -> instformat record*)
let ins2instformat (i: inst) : instformat = 
  match i with
  | Add (rd, rs, rt) -> 
      R {r_opcode=0l;r_rs=reg2ind32 rs;r_rt=reg2ind32 rt;r_rd=reg2ind32 rd;
      r_shamt=0l;r_fun=0x20l}
  | Beq (rs, rt, offset) ->
      I {i_opcode=0x4l;i_rs=reg2ind32 rs;i_rt=reg2ind32 rt;i_imm=offset}
  | Jr rs ->
      R {r_opcode=0l;r_rs=reg2ind32 rs;r_rt=0l;r_rd=0l;r_shamt=0l;r_fun=0x8l}
  | Jal addr -> 
      J {j_opcode=0x3l;j_addr=addr}
  | Li (rd, imm) -> raise (PError "Li encountered")
  | Lui (rt, imm) ->
      I {i_opcode=0xfl;i_rs=0l;i_rt=reg2ind32 rt;i_imm=imm}
  | Ori (rt, rs, imm) ->
      I {i_opcode=0xdl;i_rs=reg2ind32 rs;i_rt=reg2ind32 rt;i_imm=imm}
  | Lw (rt, rs, imm) ->
      I {i_opcode=0x23l;i_rs=reg2ind32 rs;i_rt=reg2ind32 rt;i_imm=imm}
  | Sw (rt, rs, imm) ->
      I {i_opcode=0x2bl;i_rs=reg2ind32 rs;i_rt=reg2ind32 rt;i_imm=imm}
;;

let ind2reg (i:int32) : reg =
  str2reg ("$"^(Int32.to_string i))
;;
let instformat2ins (f: instformat) : inst =
  match f with
  | R rfor -> 
    (match (rfor.r_fun) with
    | 0x20l -> (Add ((ind2reg rfor.r_rd),(ind2reg rfor.r_rs),
                (ind2reg rfor.r_rt)))
    | 0x8l ->  (Jr (ind2reg rfor.r_rs))
    | _ -> raise FatalError
  )
  | I ifor -> let regs = ind2reg ifor.i_rs in
              let regt = ind2reg ifor.i_rt in
              let imm32 = ifor.i_imm in
    (match (ifor.i_opcode) with
    | 0x4l -> Beq (regs, regt, imm32)
    | 0xfl -> Lui (regt,imm32)
    | 0xdl -> Ori (regt, regs, imm32)
    | 0x23l -> Lw (regt, regs, imm32)
    | 0x2bl -> Sw (regt, regs, imm32)
    | _ -> raise FatalError
  )
  | J jfor -> Jal ((jfor.j_addr))
;;

(*Fuse together a list of ints into a word by squeezing them into bit segments*)
(*We need to mask all numbers to have the right width before squeezing*)
let combine_bits (bl: (int32*int) list) : int32 (*bits, bit width*) =
  let rec combine_bits_offset (bl': (int32*int) list) (offset:int) : int32 = 
    match bl' with
    | (b1,l)::tl -> (Int32.add 
      (Int32.shift_left (Int32.logand (ones l) b1) (offset-l))
      (combine_bits_offset tl (offset-l))
    )
    | [] -> 0l
  in
  combine_bits_offset bl 32
;;

let instformat2word (f: instformat) : int32 =
  match f with
  | R rfor -> combine_bits[(rfor.r_opcode,6);(rfor.r_rs,5);
  (rfor.r_rt,5);(rfor.r_rd,5);(rfor.r_shamt,5);(rfor.r_fun,6)]
  | I ifor -> combine_bits[(ifor.i_opcode,6);(ifor.i_rs,5);
  (ifor.i_rt,5);(ifor.i_imm,16)]
  | J jfor -> combine_bits[(jfor.j_opcode,6);(jfor.j_addr,26)]
;;

let ins2word (i: inst) : int32 = instformat2word (ins2instformat i)
;;

let instformat_to_string f = match f with
| R rs -> ("R"^(Int32.to_string rs.r_opcode))
| I is -> ("I"^(Int32.to_string is.i_opcode))
| J js -> ("J"^(Int32.to_string js.j_opcode))
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
;;

(*get byte 0,1,2,3 of a word*)
let getByte (w: int32) (j: int) : byte =
  mk_byte (Int32.shift_right_logical w (24-8*j))
;;


let rec assem_code (prog : program) (offset : int32) 
  (init_mem: memory) : memory = snd (List.fold_left 
  (fun (s:(int32*memory)) (i:inst)->
    let curWord = ins2word i in
    let curPos = fst s in
    let curMem = snd s in
    (Int32.add curPos 4l,
      mem_update (curPos) (getByte curWord  0) (
      mem_update (Int32.add curPos 1l) (getByte curWord 1) (
      mem_update (Int32.add curPos 2l) (getByte curWord 2) (
      mem_update (Int32.add curPos 3l) (getByte curWord 3) (
        curMem))))
      )
  )
  (init_pc,init_mem) (rem_pseudo prog))
;;

let rec assem (prog : program) : state = 
  {r=init_regfile;pc=init_pc;m=assem_code prog init_pc empty_mem}
;;

(*********** Assembler Tests ***************)

open Format

(* Encoding tests *)
let e_add = Add(R1,R2,R3)
let e_beq = Beq(R4,R5,0x1l)
let e_jr = Jr(R7)
let e_jal = Jal(0xADBEEFl)
let e_li = Li(R8,0xFEEDFACEl)
let e_lui = Lui(R9,0xFEEDl)
let e_ori = Ori(R10,R11,0xFACEl)
let e_lw = Lw(R12,R13,0x4l)
let e_sw = Sw(R14,R15,0x100l)

(* Expected results (little endian, big endian) *)
let e_add_res = (0x00430820l, 0x20084300l)
let e_beq_res = (0x10850001l, 0x1008510l)
let e_jr_res = (0x00E00008l, 0x800E000l)
let e_jal_res = (0x0CADBEEFl, 0xEFBEAD0Cl)
let e_lui_res = (0x3C09FEEDl, 0xEDFE093Cl)
let e_ori_res = (0x356AFACEl, 0xCEFA6A35l)
let e_lw_res = (0x8DAC0004l, 0x400AC8Dl)
let e_sw_res = (0xADEE0100l, 0x1EEADl)

(* 4 bytes to Int32 *)
let b2i b1 b2 b3 b4 =
  let t1 = b2i32 b1 in
  let t2 = Int32.add (Int32.shift_left (b2i32 b2) 8) t1 in
  let t3 = Int32.add (Int32.shift_left (b2i32 b3) 16) t2 in
  Int32.add (Int32.shift_left (b2i32 b4) 24) t3

(* read a word from memory *)
(* IMPORTANT: this is reading a little endian encoding, use
   rev_endianess to convert to big endian instead *)
let read_word (mem : memory) (addr : int32) =
  let b1 = mem_lookup addr mem in
  let b2 = mem_lookup (Int32.add addr 0x1l) mem in
  let b3 = mem_lookup (Int32.add addr 0x2l) mem in
  let b4 = mem_lookup (Int32.add addr 0x3l) mem in
  b2i b1 b2 b3 b4

(* reverse the endianness of an Int32 *)
let rev_endianess w = 
  let b1 = Int32.shift_right (Int32.logand w 0xFF000000l) 24 in
  let b2 = Int32.shift_right (Int32.logand w 0x00FF0000l) 16 in
  let b3 = Int32.shift_right (Int32.logand w 0x0000FF00l) 8 in
  let b4 = Int32.logand w 0x000000FFl in
  b2i (mk_byte b1) (mk_byte b2) (mk_byte b3) (mk_byte b4)

(* encode an instruction *)
let encode inst = 
  let state = assem inst in
  read_word state.m state.pc

(* test an encoding *)
let teste i (s,b) = 
  let encoding = encode i in
  if (encoding = s) || (encoding = b) 
  then print_string ((format_string "passed: " Bright Green)^prog2str(i)) 
  else print_string ((format_string "failed: " Bright Red)^(tostring encoding)^"   "^prog2str(i))

let test_encoding () =
  let _ = teste [e_add] e_add_res in
  let _ = teste [e_beq] e_beq_res in
  let _ = teste [e_jr] e_jr_res in
  let _ = teste [e_jal] e_jal_res in
  let _ = teste [e_lui] e_lui_res in
  let _ = teste [e_ori] e_ori_res in
  let _ = teste [e_lw] e_lw_res in
  let _ = teste [e_sw] e_sw_res in
();;

