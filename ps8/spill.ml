open Cfg_ast
module CG = Cfg_ast

exception SpillError of string

let rec lookoffset (v:var) (sl:var list) : int =
  match sl with
    | hd::tl -> (if v=hd then 0 else 1+(lookoffset v tl))
    | [] -> raise (SpillError "var not found")

let spillcount = ref 0
let new_spill_temp () = (
  spillcount := (!spillcount) + 1;
  "t"^(string_of_int (!spillcount))
)


let spill_var_inst (i:CG.inst) (sl:var list) (v:var) : CG.inst list =
  (*First rewrite any defs*)
  let (i_after_def_rewrite,storeinst) =
    let defvar : bool =
      (match i with
        | CG.Move (CG.Var vd,_) -> vd = v
        | CG.Arith (CG.Var vd,_,_,_) -> vd = v
        | CG.Load (CG.Var vd,_,_) -> vd=v
        | _ -> false)
    in
    let repdefvar () : (CG.inst*(CG.inst list)) =
      let vnew = new_spill_temp() in
      let newinst =
        match i with
          | CG.Move (CG.Var vd,o2) ->
              CG.Move (CG.Var vnew,o2)
          | CG.Arith (CG.Var vd,o2,aop,o3) ->
              CG.Arith (CG.Var vnew,o2,aop,o3)
          | CG.Load (CG.Var vd,op2,i) ->
              CG.Load (CG.Var vnew,op2,i)
          | _ -> raise (SpillError "repdefvar match")
      in
        (newinst,[Store (sp,4*(lookoffset v sl),Var vnew)])
    in
      if (defvar) then (repdefvar ()) else (i,[])
  in

  (*Then rewrite any uses*)
  let i2 = i_after_def_rewrite in
  let (i_after_use_rewrite,loadinst) =
    let opusevar (o:CG.operand) : bool =
      match o with
        | Var ov -> ov=v
        | _ -> false
    in
    let usevar : bool =
      match i2 with
        | CG.Move (_,CG.Var vs) -> vs=v
        | CG.Arith (_,op1,_,op2) -> (opusevar op1) || (opusevar op2)
        | CG.Load (_,CG.Var vs,_) -> vs=v
        | CG.Store (op1,_,op2) -> (opusevar op1) || (opusevar op2)
        | CG.Call (CG.Var vs) -> vs=v
        | CG.If (op1,_,op2,_,_) -> (opusevar op1) || (opusevar op2)
        | _ -> false
    in
    let repusevar () : (CG.inst*CG.inst list) =
      let vnew = new_spill_temp() in
      let repusevar_op (o:CG.operand) : CG.operand =
        match o with
          | Var ov -> if(ov=v) then Var vnew else Var ov
          | _ -> o
      in
      let newinst =
        match i2 with
          | CG.Move (o1,_) -> CG.Move(o1,CG.Var vnew)
          | CG.Arith (opd,op1,aop,op2) -> 
              CG.Arith(opd,repusevar_op op1,aop,repusevar_op op2)
          | CG.Load (opd,_,ioff) -> CG.Load(opd,CG.Var vnew,ioff)
          | CG.Store (op1,ioff,op2) -> 
              CG.Store(repusevar_op op1,ioff,repusevar_op op2)
          | CG.Call _ -> CG.Call(CG.Var vnew)
          | CG.If (op1,cop,op2,l1,l2) ->
              CG.If (repusevar_op op1,cop,repusevar_op op2,l1,l2)
          | _ -> raise (SpillError "repusevar match")
      in
        (newinst,[Load (Var vnew,sp,4*(lookoffset v sl))])
    in
      if usevar then (repusevar ()) else (i2,[])
  in
    (List.append loadinst (i_after_use_rewrite::storeinst))
;;

(*Rewrites a raw f with no spills to spill the vars in sl*)
let spill (forig:func) (sl:var list) : func = 
  let spdelta = 4*List.length sl in
  (*Prologue and epilogue are for manipulating the sp to allow for stack
   * storage*)
  let prologue = CG.Arith (CG.sp,CG.sp,CG.Minus,Int spdelta) in
  let epilogue = CG.Arith (CG.sp,CG.sp,CG.Plus,Int spdelta) in
  let addprologue (b:block) : block =
    (List.hd b)::(prologue)::(List.tl b) in
  let addepilogue (b:block) : block =
    let isnotret i = match i with
      | CG.Return -> false
      | _ -> true
    in
    let (bfront,bback) = List.partition isnotret b in
      List.append bfront (epilogue::bback)
  in
  let spill_var (f:func) (v:var) : func =
    let spill_var_block (b:block) : block =
      List.fold_right (fun i brest -> List.append (spill_var_inst i sl v) brest) b []
    in
      List.map (spill_var_block) f
  in

  (*Call this BEFORE spilling anything, to change the offsets of stack allocated
   * args*)
  let rewrite_stack_args : func =
    let rsa_inst (i:CG.inst) : CG.inst =
      match i with
        | CG.Load (opd,ops,ioff) -> 
            if(ops=CG.sp) then
              CG.Load(opd,CG.sp,ioff+spdelta)
            else i
        | _ -> i
    in
    let rsa_block (b:block) : block =
      List.fold_right (fun i brest -> (rsa_inst i)::brest) b [] in
      List.map (rsa_block) forig
  in
  let spilled_body = List.fold_left (fun curf curv -> spill_var curf curv)
                       rewrite_stack_args sl in

  (*Rewrite spilled temps that we jalr to, that will appear after the sp is
   * decremented. *)
  let rewrite_jalr_stack : func =
    let rec rewrite_jalr_stack_block (b:block) :block =
      match b with
        | (CG.Arith(opstk1,opstk2,Minus,Int opstkdlt))::(CG.Load
                                                           (opcalld1,opstk3,i))::(CG.Call opcalld2)::tl ->
            let newi = 
              if(opstk1=CG.sp && opstk2=opstk1 && opstk3=opstk1) then
                (i+opstkdlt) else i in
              (CG.Arith(CG.sp,CG.sp,Minus,Int opstkdlt))::
              (CG.Load(opcalld1,opstk3,newi))::
              (CG.Call opcalld2)::
              (rewrite_jalr_stack_block tl)
        | hd::tl -> hd::(rewrite_jalr_stack_block tl)
        | [] -> []
    in
      List.map (rewrite_jalr_stack_block) spilled_body
  in
  let repbegin (f:func) = (addprologue (List.hd f))::(List.tl f) in
  let rec repend (f:func) =
    match f with
      | hd1::hd2::tl -> hd1::(repend (hd2::tl))
      | last::[] -> (addepilogue last)::[]
      | [] -> raise (SpillError "no end in repend")
  in
    repbegin (repend (rewrite_jalr_stack))

let vl2str (sl : var list) : string =
  List.fold_right (fun v s -> v^","^s) sl ""
