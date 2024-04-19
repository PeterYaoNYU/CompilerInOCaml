(* Graph interface from Appel *)

open Map
open Set

module type DIRECTED_GRAPH =
  functor (Elt: OrderedType) ->
    sig
      type node = Elt.t
      type graph
      val nodes: graph -> Set.Make(Elt).t
      (* succ u g returns a set that contains the successors
         of u *)
      val succ : node -> graph -> Set.Make(Elt).t

      (* pred u g returns a set of predecessors of u *)
      val pred : node -> graph -> Set.Make(Elt).t

      (* adj u g is a set of all nodes that are either preds/succs of u in g *)
      val adj : node -> graph -> Set.Make(Elt).t

      val empty : graph

      val addNode : node -> graph -> graph

      val rmNode : node -> graph -> graph 

      (* addEdge u v g adds directed edge u->v to g. Note that
         if u or v are not in the graph they are added
      *) 
      val addEdge : node -> node -> graph -> graph

      (* rmEdge u v g removes the directed edge u->v from g. *)
      val rmEdge : node -> node -> graph -> graph

    end;;

(* 
   Implementation that uses pairs of maps for succ/pred
   relations.
*) 

module DirectedGraph_Raw =
  functor (Elt: OrderedType) ->
    struct 
      module NodeMap = Map.Make(Elt)
      module NodeSet = Set.Make(Elt)
      type node = Elt.t
      type graph = {nodes: NodeSet.t; succ: NodeSet.t NodeMap.t;
                    pred: NodeSet.t NodeMap.t}
      let nodes (g: graph) = g.nodes
      let succ n g = NodeMap.find n g.succ
      let pred n g = NodeMap.find n g.pred
      let adj n g = NodeSet.union (succ n g) (pred n g)
      let empty = {nodes = NodeSet.empty; succ = NodeMap.empty;
                   pred = NodeMap.empty}

      let addNode n g = {nodes = NodeSet.add n g.nodes; 
                         succ = if (NodeMap.mem n (g.succ)) then
                           g.succ
                         else
                           NodeMap.add n (NodeSet.empty) g.succ;
                         pred = if (NodeMap.mem n (g.pred)) then
                           g.pred
                         else
                           NodeMap.add n (NodeSet.empty) g.pred}

      let addEdge u v g = 
        let g = addNode u (addNode v g) in
        let succ' = if(NodeMap.mem u (g.succ)) then
                       NodeMap.add u (NodeSet.add v (succ u g)) (g.succ)
                     else
                       NodeMap.add u (NodeSet.singleton v) (g.succ) in
        let pred' = if(NodeMap.mem v (g.pred)) then
                       NodeMap.add v (NodeSet.add u (pred v g)) (g.pred)
                     else
                       NodeMap.add v (NodeSet.singleton u) (g.pred) in
        {nodes = g.nodes; succ = succ'; pred = pred'}
      
      let rmEdge u v g = 
        (* note that unlike addEdge, rmEdge will not modify node list *)
        let succ' = if(NodeMap.mem u (g.succ)) then
                       NodeMap.add u (NodeSet.remove v (succ u g)) (g.succ)
                     else
                       g.succ in
        let pred' = if(NodeMap.mem v (g.pred)) then
                       NodeMap.add v (NodeSet.remove u (pred v g)) (g.pred)
                     else
                       g.pred in
        {nodes = g.nodes; succ = succ'; pred = pred'}

      let rmNode n g =
        let g' = NodeSet.fold (fun x acc -> rmEdge n x acc) (succ n g) g in
	let g''= NodeSet.fold (fun x acc -> rmEdge x n acc) (pred n g') g' in
	{nodes = NodeSet.remove n g''.nodes; succ = g''.succ; pred = g''.pred}

    end;;


module DirectedGraph = (DirectedGraph_Raw: DIRECTED_GRAPH);;

module type UNDIRECTED_GRAPH =
  functor (Elt: OrderedType) ->
    sig
      type node = Elt.t
      type graph
      val nodes: graph -> Set.Make(Elt).t

      (* adj u g is a set of all nodes that are adjacent to u in g *)
      val adj : node -> graph -> Set.Make(Elt).t

      val empty : graph

      (* adding and removing nodes from a graph *)
      val addNode : node -> graph -> graph
      val rmNode : node -> graph -> graph

      val degree : node -> graph -> int

      (* addEdge u v g adds undirected edge between u and v to g. Note that
         if u or v are not in the graph they are added
      *) 
      val addEdge : node -> node -> graph -> graph

      (* rmEdge u v g removes the edge between u and v from g. *)
      val rmEdge : node -> node -> graph -> graph

    end;;

(* 
   Implementation that uses adjacency maps 
*) 

module UndirectedGraph_Raw =
  functor (Elt: OrderedType) ->
    struct 
      module NodeMap = Map.Make(Elt)
      module NodeSet = Set.Make(Elt)
      type node = Elt.t
      type graph = {nodes: NodeSet.t; adj: NodeSet.t NodeMap.t}
      let nodes (g: graph) = g.nodes
      let adj n g = NodeMap.find n g.adj
      let empty = {nodes = NodeSet.empty; adj = NodeMap.empty;}
      let addNode n g = {nodes = NodeSet.add n g.nodes;
                         adj = if (NodeMap.mem n (g.adj)) then
                           g.adj
                         else
                           NodeMap.add n (NodeSet.empty) g.adj}

      let addEdge u v g = 
        let g = addNode u (addNode v g) in
        let adj' = if(NodeMap.mem u (g.adj)) then
                       NodeMap.add u (NodeSet.add v (adj u g)) (g.adj)
                     else
                       NodeMap.add u (NodeSet.singleton v) (g.adj) in
        let adj'' = if(NodeMap.mem v (adj')) then
                       NodeMap.add v (NodeSet.add u (NodeMap.find v adj')) (adj')
                     else
                       NodeMap.add v (NodeSet.singleton u) (adj') in
        {nodes = g.nodes; adj = adj''}

      let rmEdge u v g = 
        (* note that unlike addEdge, rmEdge will not modify node list *)
        let adj' = if(NodeMap.mem u (g.adj)) then
                       NodeMap.add u (NodeSet.remove v (adj u g)) (g.adj)
                     else
                       g.adj in
        let adj'' = if(NodeMap.mem v (adj')) then
                       NodeMap.add v (NodeSet.remove u (NodeMap.find v adj')) (adj')
                     else
                       adj' in
        {nodes = g.nodes; adj = adj''}

      let degree n g =
        if NodeMap.mem n g.adj then
          NodeSet.cardinal (adj n g) 
        else
          0


      let rmNode n g =
	let g' = NodeSet.fold (fun x acc -> rmEdge n x acc) (adj n g) g in
	{nodes = NodeSet.remove n g'.nodes; adj = g'.adj}

    end;;

module UndirectedGraph = (UndirectedGraph_Raw: UNDIRECTED_GRAPH);;

