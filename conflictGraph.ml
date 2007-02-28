(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in file LICENSE.                                     *)
(*                                                                        *)
(**************************************************************************)

open Grammar
open Lr1

(* ------------------------------------------------------------------------ *)

(* A simple representation of graphs. *)

type graph = {
    nodes: NodeSet.t;
    entry: NodeSet.t;
    edges: NodeSet.t NodeMap.t;
    reverse_edges: NodeSet.t NodeMap.t;
  }

let successors graph x =
  try
    NodeMap.find x graph.edges
  with Not_found ->
    assert false

let predecessors graph x =
  try
    NodeMap.find x graph.reverse_edges
  with Not_found ->
    NodeSet.empty

(* ------------------------------------------------------------------------ *)

(* Building a graph out of the automaton. *)

let create () : graph =
  {
    nodes =
      Lr1.fold (fun nodes x ->
	NodeSet.add x nodes
      ) NodeSet.empty;
    entry =
      Array.fold_right NodeSet.add Lr1.entry NodeSet.empty;
    edges =
      Lr1.fold (fun edges x ->
	let successors =
	  SymbolMap.fold (fun _ y successors ->
	    NodeSet.add y successors
	  ) (Lr1.transitions x) NodeSet.empty
	in
	NodeMap.add x successors edges
      ) NodeMap.empty;
    reverse_edges =
      Lr1.fold (fun reverse_edges x ->
	SymbolMap.fold (fun _ y reverse_edges ->
	  let predecessors =
	    try
	      NodeMap.find y reverse_edges
	    with Not_found ->
	      NodeSet.empty
	  in
	  NodeMap.add y (NodeSet.add x predecessors) reverse_edges
        ) (Lr1.transitions x) reverse_edges
      ) NodeMap.empty
  }

(* ------------------------------------------------------------------------ *)

(* This function removes a node, linking its predecessors directly to
   its successors. If the node was an entry point, its successors
   become entry points. *)

let remove graph x =
  let successors = successors graph x
  and predecessors = predecessors graph x in
  let forward replacements xs =
    if NodeSet.mem x xs then
	NodeSet.remove x (NodeSet.union replacements xs)
      else
	xs
  in
  {
    nodes =
      NodeSet.remove x graph.nodes;
    entry =
      forward successors graph.entry;
    edges =
      NodeMap.map (forward successors) graph.edges;
    reverse_edges =
      NodeMap.map (forward predecessors) graph.reverse_edges;
  }

(* ------------------------------------------------------------------------ *)

(* This function removes all non-conflict nodes. *)

let simplify graph =
  
  (* First, build a set of all conflict nodes. *)

  let conflict_nodes =
    ref NodeSet.empty
  in
  Lr1.conflicts (fun _ node ->
    conflict_nodes := NodeSet.add node !conflict_nodes
  );

  (* Second, remove all non-conflict nodes. *)

  Lr1.fold (fun graph node ->
    if NodeSet.mem node !conflict_nodes then
      graph
    else
      remove graph node
  ) graph

(* ------------------------------------------------------------------------ *)

(* This function writes a graph to a file. *)

(* TEMPORARY which states are entry points is not reflected *)

let print channel graph =

  let module P = Dot.Print (struct

    type vertex =
	node

    let name node =
      Printf.sprintf "s%d" (Lr1.number node)

    let successors (f : ?style:Dot.style -> label:string -> vertex -> unit) x =
      NodeSet.iter (fun y ->
	f ~label:"" y
      ) (successors graph x)

    let iter (f : ?style:Dot.style -> label:string -> vertex -> unit) =
      NodeSet.iter (fun x ->
	if NodeSet.mem x graph.entry then
	  f ~style:Dot.Bold ~label:(Printf.sprintf "%d" (Lr1.number x)) x
	else
	  f ~label:(Printf.sprintf "%d" (Lr1.number x)) x
      ) graph.nodes

  end) in

  P.print channel

(* ------------------------------------------------------------------------ *)

(* If required, create the conflict graph and write it to the
   .conflicts.dot file. *)

let () =
  if Settings.explain then
    let f = open_out (Settings.base ^ ".conflicts.dot") in
    print f (simplify (create ()));
    close_out f;
    Time.tick "Creating and writing down the conflict graph"

