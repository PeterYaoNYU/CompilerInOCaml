open Cfg_ast
exception Implement_Me
exception FatalError
exception BlockError 


type igraph_node = RegNode of Mips.reg | VarNode of var

(* These are the registers that must be generated / killed as part of
   liveness analysis for call instructions to reflect MIPS calling
   conventions *)

let call_gen_list = ["$4";"$5";"$6";"$7"]
let call_kill_list = ["$1";"$2";"$3";"$4";"$5";"$6";"$7";"$8";"$9";"$10";
                      "$11";"$12";"$13";"$14";"$15";"$24";"$25";"$31"]

let string2reg (s:string) :Mips.reg =
  match s with
  | "$0" -> R0   | "$1" -> R1   | "$2" -> R2   | "$3" -> R3
  | "$4" -> R4   | "$5" -> R5   | "$6" -> R6   | "$7" -> R7
  | "$8" -> R8   | "$9" -> R9   | "$10" -> R10 | "$11" -> R11
  | "$12" -> R12 | "$13" -> R13 | "$14" -> R14 | "$15" -> R15
  | "$16" -> R16 | "$17" -> R17 | "$18" -> R18 | "$19" -> R19
  | "$20" -> R20 | "$21" -> R21 | "$22" -> R22 | "$23" -> R23
  | "$24" -> R24 | "$25" -> R25 | "$26" -> R26 | "$27" -> R27
  | "$28" -> R28 | "$29" -> R29 | "$30" -> R30 | "$31" -> R31
  | _ -> failwith ("Unknown MIPS register: " ^ s)

let call_gen_list_reg : Mips.reg list = List.map string2reg call_gen_list
let call_kill_list_reg : Mips.reg list = List.map string2reg call_kill_list

(* Below are helpers for making the flow graph *)

type flow_graph_node = Block of block

module FGraphNode =
  struct
    type t = flow_graph_node
    let compare = compare
  end

module FlowNodeSet = Set.Make(FGraphNode)

module FGraph = Graph.DirectedGraph(FGraphNode)

type flow_graph = FGraph.graph

module BlockMap = Map.Make(FGraphNode)
(* 
let def_map : igraph_node list BlockMap.t = BlockMap.empty

let use_map : igraph_node list BlockMap.t = BlockMap.empty *)

let find_block_with_label (f: func) (label: label) : flow_graph_node =
  let rec find_block_with_label' (f: func) (label: label) : block =
    match f with
    | [] -> raise FatalError
    | head_block :: t -> 
      (
        match head_block with

        | (Label l) :: _ when l = label -> head_block
        | _ :: _ -> find_block_with_label' t label
        | [] -> raise FatalError
      )
  in
  Block (find_block_with_label' f label)


let make_graph (f: func) : flow_graph = 
  let graph = FGraph.empty in
  let graph_with_nodes = List.fold_left (fun acc_graph block -> FGraph.addNode (Block block) acc_graph) graph f in
  
  let add_edges acc_graph block = 
    match List.rev block with 
    | (Jump label) :: _ -> 
      let target_block_node = find_block_with_label f label in
      FGraph.addEdge (Block block) target_block_node acc_graph
    | (If (_, _, _, label_true, label_false)) :: _ -> 
      let true_block_node = find_block_with_label f label_true in
      let false_block_node = find_block_with_label f label_false in
      let graph_with_true_edge = FGraph.addEdge (Block block) true_block_node acc_graph in
      FGraph.addEdge (Block block) false_block_node graph_with_true_edge
    | Return :: _ -> acc_graph
    | _ -> raise BlockError
  in
  List.fold_left add_edges graph_with_nodes f

let update_maps block (def_map, use_map) = 
  let process_instruction def use inst = 
    match inst with 
     | Move (Var v, _) | Arith (Var v, _, _, _) -> (VarNode v::def, use)
     | Move (_, Var v) | Arith (_, Var v, _, _) | Arith (_, _, _, Var v) -> (def, VarNode v::use)
     | If (Var v, _, Var w, _, _) -> (def, VarNode v :: VarNode w :: use)
     | Load (Var v, _, _) -> (VarNode v :: def, use)
     | Load (_, Var v, _) -> (def, VarNode v :: use)
     | Store (_, _, Var v) -> (def, VarNode v :: use)
     | Call (_) -> ((List.append (List.map (fun x -> RegNode x) call_kill_list_reg) def), (List.append (List.map (fun x -> RegNode x) call_gen_list_reg) use))
     | _ -> (def, use)
  in
  let def, use = List.fold_left (fun (def_acc, use_acc) inst -> process_instruction def_acc use_acc inst) ([], []) block in
  let block_node = Block block in
  (BlockMap.add block_node def def_map, BlockMap.add block_node use use_map)

let build_maps (f: func) = List.fold_left (fun maps block -> update_maps block maps) (BlockMap.empty, BlockMap.empty) f

  

    
  



(* Above are helpers for making the flow graph *)

let string_of_node (n: igraph_node) : string =
  match n with
  | RegNode r -> Mips.reg2string r
  | VarNode v -> v
;;

module IGraphNode =
  struct
    type t = igraph_node
    let compare = compare
  end

module NodeSet = Set.Make(IGraphNode)                                                   


(* Undirected graphs where nodes are identified by igraph_node type above. Look at
   graph.ml for the interface description.  *)

module IUGraph = Graph.UndirectedGraph(IGraphNode)

(* this is a wrapper to addEdge that prevents adding self edges.
   to do all sorts of other complicated stuff for eg coloring *)
let specialAddEdge u v g =
  if (u = v) then
    g
  else
    IUGraph.addEdge u v g

(* An interference graph is an SUGraph where a node is temp variable
   or a register (to be able to handle pre-colored nodes)

   The adjacency set of variable x should be the set of variables
   y such that x and y are live at the same point in time. *)
type interfere_graph = IUGraph.graph

(* To help you printing an igraph for debugging *)
let string_of_igraph (g: interfere_graph) : string =
  let rec string_of_row (n: IUGraph.node) =
    let ns = IUGraph.adj n g in
    Printf.sprintf "%s : {%s}"
      (string_of_node n)
      (String.concat "," (List.map string_of_node (NodeSet.elements ns)))
  in
  let rows = String.concat "\n" (List.map string_of_row (NodeSet.elements (IUGraph.nodes g))) in
  Printf.sprintf "{\n%s\n}" rows
  

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (f : func) : interfere_graph = 
    raise Implement_Me
