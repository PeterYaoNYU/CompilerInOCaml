open Mips
open Cfg_ast
open Cfg

exception AllocError of string
exception Implement_Me

module NodeMap = Map.Make(Cfg.IGraphNode)    

(* module NodeSet = Set.Make(IGraphNode)                                                    *)

module RegSet = Set.Make(struct
                  type t = reg
                  let compare = fun x y -> (String.compare (reg2string x) (reg2string y))
                end)
let reglist2sset ls =
  List.fold_left (fun ls var -> RegSet.add var ls) RegSet.empty ls

let validreglist = 
  R2 :: R3  :: R4 :: R5 ::   R6 ::  R7  ::  R8 :: R9 ::  R10 :: 
  R11 :: R12 :: R13 :: R14 :: R15 :: R16 :: R17 :: R18 :: R19 :: R20 ::
  R21 :: R22 :: R23 (*R24,R25 reserved*) :: R30 :: R31 :: []
let regcount = List.length validreglist
let validregset = reglist2sset validreglist 

let rewriteOp colormap o =
  match o with
    | Var x -> 
        (match NodeMap.find (Cfg.VarNode x) colormap with
          | None -> raise (AllocError "No color assigned for that variable.") 
          | Some r -> Cfg_ast.Reg r)
    | _ -> o

let rewriteInst colormap (i: Cfg_ast.inst) : Cfg_ast.inst = 
  let rewrite = rewriteOp colormap in
    match i with
      | Label x -> Label x
      | Move (x, y) -> Move (rewrite x, rewrite y)
      | Arith (x, y, p, z) -> Arith (rewrite x, rewrite y, p, rewrite z)
      | Load (x, y, i) -> Load (rewrite x, rewrite y, i)
      | Store (x, i, y) -> Store (rewrite x, i, rewrite y)
      | Call x -> Call (rewrite x)
      | Jump x -> Jump x
      | If (x,b,y,t,f) -> If (rewrite x, b, rewrite y, t, f)
      | Return -> Return


type colormap = ((RegSet.elt option) NodeMap.t)

type assign_result = 
    | Success of colormap
    | Fail of var list

(*******************************************************************)
(* PS8 TODO:  graph coloring *)

(* given an inteference graph, return an assign_result, which consists of either
   (1) Success cm, where cm is a coloring, i.e. a map from variables to registers
   (2) Fail vs, where vs is a list of variables to spill, when a graph coloring could not be found

 *)

let assign_colors (ig: Cfg.interfere_graph) f : assign_result =
  let num_regs = regcount in
  let stack = ref [] in 
  let coloring = ref NodeMap.empty in 

  let push_node n = stack := n::!stack in 
  let pop_node() = 
    match !stack with 
    | [] -> raise (AllocError "Empty stack")
    | hd :: tl -> stack := tl; hd
  in

  let rec simplify_graph nodes = 
    try 
    let (node, degree) = 
      NodeSet.fold (fun n (acc_n, acc_d) -> 
        let d = IUGraph.degree n ig in
        if d < acc_d && d < num_regs then (Some n, d) else (acc_n, acc_d)  
      ) nodes (None, num_regs)
    in
    match node with 
    | None -> ()
    | Some n ->
      IUGraph.rmNode n ig;
      push_node n;
      simplify_graph (NodeSet.remove n nodes)
    with _ -> raise (AllocError "Error in simplify_graph")
  in 

  let all_nodes = IUGraph.nodes ig in 
  simplify_graph all_nodes;

  let available_colors = RegSet.elements validregset in
  while !stack <> [] do
    let node = pop_node() in
    let neighbors = IUGraph.adj node ig in
    let forbidden_colors = 
      NodeSet.fold (fun n acc ->
        match NodeMap.find n !coloring with
        | Some c -> RegSet.add c acc
        | None -> acc
      ) neighbors RegSet.empty
    in
    let rec assing_color = function 
      | [] -> raise (AllocError "Not enough colors")
      | h :: t -> 
        if RegSet.mem h forbidden_colors then 
          assing_color t
        else
          begin
            coloring := NodeMap.add node (Some h) !coloring;
          end
    in
    assing_color available_colors
  done;

  if NodeSet.cardinal (IUGraph.nodes ig) > 0 then 
    Fail (List.map (fun n -> match n with 
                          VarNode v -> Some v 
                          | _ -> None)
              (NodeSet.elements (IUGraph.nodes ig))
              |> List.filter_map Fun.id)
else 
  Success !coloring 


let rec reg_alloc_spill (fraw : func) (sl: var list): func = 
  (*First spill all of the vars in sl by adding loads/stores to fraw*)
  let f = Spill.spill fraw sl in
  let ig = Cfg.build_interfere_graph f in
  let colormapopt = assign_colors ig f in
    match colormapopt with
      | Success colormap ->
          let allocatedf = List.map (fun x -> List.map (rewriteInst colormap) x) f in
            (*Get rid of trivial moves*)
            List.map 
              (fun b -> List.filter
                          (fun i ->
                             match i with
                               | Move (o1,o2) -> (if(o1=o2) then false else true)
                               | _ -> true
                          ) b
              )
              allocatedf
      | Fail spilllist ->
            reg_alloc_spill fraw (List.append spilllist sl)

let reg_alloc (f:func) : func =
  reg_alloc_spill f []
