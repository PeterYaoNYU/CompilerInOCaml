(*Translate cfg functions without temps (aka already allocated) to MIPS code*)

open Cfg_ast

exception CompileError of string

module CG = Cfg_ast
module M = Mips

let combineblocks (f : CG.func) : CG.block =
  List.flatten f
let inst2mips (i:CG.inst) : Mips.inst list=
  (*TEMPS USED BY CFG->MIPS ASSEMBLER*)
  let treg1 = M.R24 in 
  let treg2 = M.R25 in
  let tw = Word32.fromInt in
  let getreg (o1:CG.operand) : M.reg =
    match o1 with
    | CG.Reg r1 -> r1
    | _ -> raise (CompileError "reg arg bad")
  in
  (*To convert operands to registers, we need to load the values into
   * tmp registers beforehand and keep track of which tmps we are loading
   * into *)
  let loadopreg' (o1:CG.operand) (regused:bool) : (M.inst list * M.reg * bool) =
    let treg = if regused then treg1 else treg2 in
    match o1 with
    | CG.Reg r1 -> ([],r1,false)
    | CG.Int i -> ([M.Li (treg,(tw i))],treg,true)
    | CG.Lab l -> ([M.La (treg,l)],treg,true)
    | _ -> raise (CompileError "var in op")
  in
  let loadopreg (o1:CG.operand) : (M.inst list * M.reg) =
    let (a,b,_) = loadopreg' o1 false in
    (a,b)
  in
  let loadop2reg (o1:CG.operand) o2: (M.inst list * M.reg * M.reg) =
    let (prel1,r1,tused) = loadopreg' o1 false in
    let (prel2,r2,_) = loadopreg' o2 tused in
    (List.append prel1 prel2,r1,r2)
  in
  match i with
  | CG.Label l -> [M.Label l]
  | CG.Move (od,os) -> 
      let rd = getreg od in
      (match os with
      | CG.Int i -> M.Li (rd,tw i)
      | CG.Reg r2 -> M.Or (rd,r2,M.Reg M.R0)
      | CG.Lab l -> M.La (rd,l)
      | _ -> raise (CompileError "var in mov source")
      )::[]
  | CG.Arith (od,o2,aop,o3) ->
      let rd = getreg od in
      (match (o2,aop,o3) with
      | (o2,CG.Plus,CG.Int i3) -> 
          let (prelude,r1) = loadopreg o2 in
          List.append prelude [M.Add(rd,r1,M.Immed (tw i3))]
      | (CG.Int i2,CG.Plus,o3) ->
          let (prelude,r1) = loadopreg o3 in
          List.append prelude [M.Add(rd,r1,M.Immed (tw i2))]
      | (o2,CG.Minus,CG.Int i3) -> 
          let (prelude,r1) = loadopreg o2 in
          List.append prelude [M.Add(rd,r1,M.Immed (tw (0-i3)))]
      | _ ->
        (let (prelude,r1,r2) = loadop2reg o2 o3 in
        List.append prelude
        ((match aop with
        | CG.Plus -> M.Add(rd,r1,M.Reg r2)
        | CG.Minus -> M.Sub(rd,r1,r2)
        | CG.Times -> M.Mul(rd,r1,r2)
        | CG.Div -> M.Div(rd,r1,r2)
        )::[])
        )
      )
  | CG.Load (od,ot,i) ->
      let rd = getreg od in
      let w1 = tw i in
      let (prel,r2) = loadopreg ot in
      List.append prel [M.Lw (rd,r2,w1)]
  | CG.Store (ot,i,os) -> 
      let (prelude,rt,rs) = loadop2reg ot os in
      let w1 = tw i in
      List.append prelude
      [M.Sw (rs,rt,w1)]
  | CG.Call o1 ->
      (match o1 with
      | CG.Lab l -> [M.Jal l]
      | CG.Reg r1 -> [M.Jalr (M.R31, r1)]
      | _ -> raise (CompileError "Call of immed or var arg")
      )
  | CG.Jump l -> [M.J l]
  | CG.If (o1,cop,o2,l1,l2) ->
      let (prelude,r1,r2) = loadop2reg o1 o2 in
      List.append prelude
        ((match cop with
        | CG.Eq -> M.Beq (r1,r2,l1)
        | CG.Neq -> M.Bne (r1,r2,l1)
        | CG.Lt -> M.Blt (r1,r2,l1)
        | CG.Lte -> M.Ble (r1,r2,l1)
        | CG.Gt -> M.Bgt (r1,r2,l1)
        | CG.Gte -> M.Bge (r1,r2,l1)
        )::[M.B l2])
  | CG.Return -> [M.Jr M.R31]
let block2mips (b: CG.block) : Mips.inst list=
  List.fold_right (fun i il -> List.append (inst2mips i) il) b []

let cfg_to_mips (f : CG.func ) : Mips.inst list = 
  block2mips (combineblocks f)
