open Cfg_ast
exception Implement_Me
exception FatalError
exception BlockError 


type igraph_node = RegNode of Mips.reg | VarNode of var
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

(* debugging functions *)

let string_of_node node = 
  match node with 
  | RegNode r -> Mips.reg2string r
  | VarNode v -> v

let print_map map = 
  BlockMap.iter (fun block_node set -> 
    match block_node with
    | Block block -> 
      let block_string = "Block Node: " ^ (block2string block) in
      let nodes_string = 
        NodeSet.fold (fun node acc ->
            acc ^ (string_of_node node) ^ ", "
          ) set ""
      in
      Printf.printf "%s: %s\n" block_string nodes_string
    | _ -> raise FatalError
  ) map

let print_set set = 
  let node_string = NodeSet.fold (fun node acc -> acc ^ (string_of_node node) ^ ", ") set "" in
  Printf.printf "%s\n>>>>>>>>>>>>>>>>>>>>>>>\n" node_string

let print_blocknode block_node = 
  match block_node with 
  | Block block -> print_endline (block2string block)
  | _ -> raise FatalError

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

let add_node_to_def def node = NodeSet.add node def
let add_node_to_use use node = NodeSet.add node use

let process_instruction (def, use) inst = 
  match inst with 
  | Move (dest, src) ->
    let use = (
      match dest with 
      | Var v -> (if (NodeSet.mem (VarNode v) use) then NodeSet.remove (VarNode v) use else use)
      | Reg r -> (if (NodeSet.mem (RegNode r) use) then NodeSet.remove (RegNode r) use else use)
      | _ -> use
    )
    in
    let def = (
      match src with
      | Var v -> (if (NodeSet.mem (VarNode v) def) then NodeSet.remove (VarNode v) def else def)
      | Reg r -> (if (NodeSet.mem (RegNode r) def) then NodeSet.remove (RegNode r) def else def)
      | _ -> def
    )
    in
    let (def, use) = (
      match dest with 
      | Var v -> (add_node_to_def def (VarNode v), use)
      | Reg r -> (add_node_to_def def (RegNode r), use)
    ) in
    let (def, use) = (
      match src with 
      | Var v -> (def, add_node_to_use use (VarNode v))
      | Reg r -> (def, add_node_to_use use (RegNode r))
      | _ -> (def, use)
    ) in
    (def, use)
  | Arith (dest, src, _, src2) -> 
    let use = (
      match dest with 
      | Var v -> (if (NodeSet.mem (VarNode v) use) then NodeSet.remove (VarNode v) use else use)
      | Reg r -> (if (NodeSet.mem (RegNode r) use) then NodeSet.remove (RegNode r) use else use)
      | _ -> use
    ) in
    let def = (
      match src with
      | Var v -> (if (NodeSet.mem (VarNode v) def) then NodeSet.remove (VarNode v) def else def)
      | Reg r -> (if (NodeSet.mem (RegNode r) def) then NodeSet.remove (RegNode r) def else def)
      | _ -> def
    ) in
    let def = (
      match src2 with
      | Var v -> (if (NodeSet.mem (VarNode v) def) then NodeSet.remove (VarNode v) def else def)
      | Reg r -> (if (NodeSet.mem (RegNode r) def) then NodeSet.remove (RegNode r) def else def)
      | _ -> def
    ) in
    let (def, use) = match dest with 
      | Var v -> (add_node_to_def def (VarNode v), use)
      | Reg r -> (add_node_to_def def (RegNode r), use)
    in
    let (def, use)= match src with 
      | Var v -> (def, add_node_to_use use (VarNode v))
      | Reg r -> (def, add_node_to_use use (RegNode r))
      | _ -> (def, use)
    in
    let (def, use) = match src2 with 
      | Var v -> (def, add_node_to_use use (VarNode v))
      | Reg r -> (def, add_node_to_use use (RegNode r))
      | _ -> (def, use)
    in
    (def, use)
  | If (src1, _, src2, _, _) -> 
    let (def, use) = (
      match src1 with 
      | Var v -> (def, add_node_to_use use (VarNode v))
      | Reg r -> (def, add_node_to_use use (RegNode r))
    ) in
    let (def, use) = (
      match src2 with 
      | Var v -> (def, add_node_to_use use (VarNode v))
      | Reg r -> (def, add_node_to_use use (RegNode r))
    ) in
    (def, use)
  | Load (dest, src, _) -> 
    let use = (
      match dest with 
      | Var v -> (if (NodeSet.mem (VarNode v) use) then NodeSet.remove (VarNode v) use else use)
      | Reg r -> (if (NodeSet.mem (RegNode r) use) then NodeSet.remove (RegNode r) use else use)
      | _ -> use
    ) in
    let def = (
      match src with
      | Var v -> (if (NodeSet.mem (VarNode v) def) then NodeSet.remove (VarNode v) def else def)
      | Reg r -> (if (NodeSet.mem (RegNode r) def) then NodeSet.remove (RegNode r) def else def)
      | _ -> def
    ) in
    let (def, use) = (
      match dest with 
      | Var v -> (add_node_to_def def (VarNode v), use)
      | Reg r -> (add_node_to_def def (RegNode r), use)
    ) in
    let (def, use) = (
      match src with 
      | Var v -> (def, add_node_to_use use (VarNode v))
      | Reg r -> (def, add_node_to_use use (RegNode r))
    ) in
    (def, use)
  | Store (_, _, src) -> 
    let def = (
      match src with
      | Var v -> (if (NodeSet.mem (VarNode v) def) then NodeSet.remove (VarNode v) def else def)
      | Reg r -> (if (NodeSet.mem (RegNode r) def) then NodeSet.remove (RegNode r) def else def)
      | _ -> def
    ) in
    let (def, use) = (
      match src with 
      | Var v -> (def, add_node_to_use use (VarNode v))
      | Reg r -> (def, add_node_to_use use (RegNode r))
    ) in
    (def, use)
    (* Pending, do not know precisely how to handle the call case *)
  | Call _ -> 
    let def = (NodeSet.union (NodeSet.of_list (List.map (fun x -> RegNode x) call_kill_list_reg)) def) in
    let use = (NodeSet.union (NodeSet.of_list (List.map (fun x -> RegNode x) call_gen_list_reg)) use) in
    (def, use)
  | _ -> (def, use)

let update_maps block (def_map, use_map) =
  let instructions = List.rev block in 
  let def, use = List.fold_left process_instruction (NodeSet.empty, NodeSet.empty) instructions in
  let block_node = Block block in
  (BlockMap.add block_node def def_map, BlockMap.add block_node use use_map)

(* Bug: the pattern matching does not fall through *)

(* let update_maps block (def_map, use_map) = 
  let process_instruction def use inst = 
    match inst with 
     | Move (Var v, _) | Arith (Var v, _, _, _) -> (NodeSet.add (VarNode v) def, use)
     | Move (Reg r, _) | Arith (Reg r, _, _, _) -> (NodeSet.add (RegNode r) def, use)
     | Move (_, Var v) | Arith (_, Var v, _, _) | Arith (_, _, _, Var v) -> (def, NodeSet.add (VarNode v) use)
     | Move (_, Reg r) | Arith (_, Reg r, _, _) | Arith (_, _, _, Reg r) -> (def, NodeSet.add (RegNode r) use)
     | If (Var v, _, Var w, _, _) -> (def, NodeSet.add (VarNode v) (NodeSet.add (VarNode w) use))
     | If (Var v, _, Reg r, _, _) -> (def, NodeSet.add (VarNode v) (NodeSet.add (RegNode r) use))
     | If (Reg r, _, Var v, _, _) -> (def, NodeSet.add (RegNode r) (NodeSet.add (VarNode v) use))
     | If (Reg r, _, Reg s, _, _) -> (def, NodeSet.add (RegNode r) (NodeSet.add (RegNode s) use))
     | Load (Var v, _, _) -> (NodeSet.add (VarNode v) def, use)
     | Load (Reg r, _, _) -> (NodeSet.add (RegNode r) def, use)
     | Load (_, Var v, _) -> (def, NodeSet.add (VarNode v) use)
     | Load (_, Reg r, _) -> (def, NodeSet.add (RegNode r) use)
     | Store (_, _, Var v) -> (def, NodeSet.add (VarNode v) use)
     | Store (_, _, Reg r) -> (def, NodeSet.add (RegNode r) use)
     | Call (_) -> (NodeSet.union (NodeSet.of_list (List.map (fun x -> RegNode x) call_kill_list_reg)) def,
                    NodeSet.union (NodeSet.of_list (List.map (fun x -> RegNode x) call_gen_list_reg)) use)
     | _ -> (def, use)
  in
  let def, use = List.fold_left (fun (def_acc, use_acc) inst -> process_instruction def_acc use_acc inst) (NodeSet.empty, NodeSet.empty) block in
  let block_node = Block block in
  (BlockMap.add block_node def def_map, BlockMap.add block_node use use_map)
*)

let build_maps (f: func) = List.fold_left (fun maps block -> update_maps block maps) (BlockMap.empty, BlockMap.empty) f 




(* Above are helpers for making the flow graph *)



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

(* my bad, it is inefficient to use ref as global variables for sets that grow larger *)
(* let live_in_sets : igraph_node list BlockMap.t = BlockMap.empty
let live_out_sets : iigraph_node list BlockMap.t = BlockMap.empty *)

(* let build_interfere_graph (f : func) : interfere_graph =  *)

let rec analyze_liveness flow_graph def_use_map live_in_sets live_out_sets =
  let changes = ref false in 

  let new_live_in_sets, new_live_out_sets = 

    FlowNodeSet.fold (fun block_node (acc_live_in_sets, acc_live_out_sets) -> 

      let gen_set = BlockMap.find block_node (snd def_use_map) in 
      let kill_set = BlockMap.find block_node (fst def_use_map) in 
      
      let live_out = FlowNodeSet.fold(fun succ live_out_acc -> 
        let succ_live_in = try BlockMap.find succ acc_live_in_sets with Not_found -> NodeSet.empty in
        NodeSet.union succ_live_in live_out_acc
      ) (FGraph.succ block_node flow_graph) NodeSet.empty in 

      print_endline ">>>>>>>>>>>>>>>>>>>>>>>live out";
      print_blocknode block_node;
      print_set live_out;

      let live_in = NodeSet.union gen_set (NodeSet.diff live_out kill_set) in
      print_endline ">>>>>>>>>>>>>>>>>>>>>>>live in";
      print_blocknode block_node;
      print_set live_in;
      let prev_live_in = try BlockMap.find block_node acc_live_in_sets with Not_found -> NodeSet.empty in
      let prev_live_out = try BlockMap.find block_node acc_live_out_sets with Not_found -> NodeSet.empty in
      if (NodeSet.equal live_out prev_live_out) then (
        print_endline ">>>>>>>>>>>>>>>>>>>>>>>nothing changes, cont to next block";
        let updated_live_in_sets = BlockMap.add block_node live_in acc_live_in_sets in 
        let updated_live_out_sets = BlockMap.add block_node live_out acc_live_out_sets in
        (updated_live_in_sets, updated_live_out_sets)
      )
      else (
        changes := true;
        let updated_live_in_sets = BlockMap.add block_node live_in acc_live_in_sets in 
        let updated_live_out_sets = BlockMap.add block_node live_out acc_live_out_sets in
        (updated_live_in_sets, updated_live_out_sets)
      )
    ) (FGraph.nodes flow_graph) (live_in_sets, live_out_sets) 
  in if !changes then analyze_liveness flow_graph def_use_map new_live_in_sets new_live_out_sets else (new_live_in_sets, new_live_out_sets)

let add_edges_to_igraph (live_in_sets: NodeSet.t BlockMap.t) (live_out_sets: NodeSet.t BlockMap.t): interfere_graph =
  let interfere_graph = IUGraph.empty in
  let add_interferences block_live_out graph = 
    NodeSet.fold (fun node acc_graph -> 
      NodeSet.fold (fun other_node acc_inner_graph ->
        specialAddEdge node other_node acc_inner_graph
      ) block_live_out acc_graph
    ) block_live_out graph
  in
  let combined_sets = BlockMap.merge (fun _ live_in live_out ->
    match live_in, live_out with
    | Some ins, Some outs -> Some (NodeSet.union ins outs)
    | Some ins, None -> Some ins
    | None, Some outs -> Some outs
    | None, None -> None
  ) live_in_sets live_out_sets in
  BlockMap.fold (fun _ combined_live_set acc_graph -> 
    add_interferences combined_live_set acc_graph
  ) combined_sets interfere_graph

let build_interfere_graph (f : func) = 
    let flow_graph = make_graph f in
    let def_use_map = build_maps f in
    print_endline ">>>>>>>>>>>>>>>>>>>>>>>";
    print_endline "Def Map:";
    print_map (fst def_use_map);
    print_endline ">>>>>>>>>>>>>>>>>>>>>>>";
    print_endline "Use Map:";
    print_map (snd def_use_map);
    print_endline ">>>>>>>>>>>>>>>>>>>>>>>";
    print_endline ">>>>>>>>>>>>>>>>>>>>>>>";
    let initial_live_in_sets = BlockMap.empty in
    let initial_live_out_sets = BlockMap.empty in
    let final_live_in_sets, final_live_out_sets = analyze_liveness flow_graph def_use_map (snd def_use_map) initial_live_out_sets in
    print_endline ">>>>>>>>>>>>>>>>>>>>>>>";
    print_endline "Live In Sets:";
    print_map final_live_in_sets;
    print_endline ">>>>>>>>>>>>>>>>>>>>>>>";
    print_endline "Live Out Sets:";
    print_map final_live_out_sets;
    let final_interfere_graph = add_edges_to_igraph final_live_in_sets final_live_out_sets in
    final_interfere_graph