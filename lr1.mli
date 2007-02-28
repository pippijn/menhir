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

(* This module constructs an LR(1) automaton by following Pager's
   method, that is, by merging states on the fly when they are found
   to be (weakly) compatible. *)

(* Shift/reduce conflicts are silently resolved when (and only when)
   that is allowed in a clean way by user-specified priorities. This
   includes shift/reduce/reduce conflicts when (and only when) there
   is agreement that the shift action should be preferred. Conflicts
   that cannot be silently resolved in this phase will be reported,
   explained, and arbitrarily resolved immediately before code
   generation. *)

(* ------------------------------------------------------------------------- *)
(* Accessors. *)

(* This is the type of the automaton's nodes. *)

type node

module Node : Set.OrderedType with type t = node

module NodeSet : Set.S with type elt = node

module NodeMap : Map.S with type key = node

(* These are the automaton's entry nodes. *)

val entry: node array

(* Nodes are numbered sequentially from [0] to [n-1]. *)

val n: int
val number: node -> int

(* This provides access to the LR(1) state that a node stands for. *)

val state: node -> Lr0.lr1state

(* This converts a start node into the single item that it contains. *)

val start2item: node -> Item.t

(* This maps a node to its incoming symbol, that is, the symbol
   carried by all of the edges that enter this node. A node has zero
   incoming edges (and, thus, no incoming symbol) if and only if it is
   a start node. *)

val incoming_symbol: node -> Symbol.t option

(* This provides access to a node's transitions and reductions. *)

val transitions: node -> node SymbolMap.t
val reductions: node -> Production.index list TerminalMap.t

(* Iteration over all nodes. *)

val fold: ('a -> node -> 'a) -> 'a -> 'a 

(* Breadth-first iteration over all edges. See [Breadth]. *)

val bfs: (bool -> node -> Symbol.t -> node -> unit) -> unit

(* Iteration over all edges that carry a certain symbol. Edges are
   grouped in families, where all edges in a single family have the
   same target node. [targets f accu symbol] invokes [f accu sources
   target] once for every family, where [sources] are the sources of
   the edges in the family and [target] is their common target. *)

val targets: ('a -> node list -> node -> 'a) -> 'a -> Symbol.t -> 'a

(* Iteration over all nodes with conflicts. [conflicts f] invokes [f
   toks node] once for every node [node] with a conflict, where [toks]
   are the tokens involved in the conflicts at that node. *)

val conflicts: (TerminalSet.t -> node -> unit) -> unit

(* [reverse_dfs goal] performs a reverse depth-first search through
   the automaton, starting at node [goal], and marking the nodes
   traversed. It returns a function that tells whether a node is
   marked, that is, whether a path leads from that node to the goal
   node. *)

val reverse_dfs: node -> (node -> bool)

(* ------------------------------------------------------------------------- *)
(* Modifications of the automaton. *)

(* This function performs default conflict resolution.

   First, it resolves standard (shift/reduce and reduce/reduce)
   conflicts (thus ensuring that the automaton is deterministic) by
   removing some reduction actions.

   Second, it resolves end-of-stream conflicts by ensuring that states
   that have a reduce action at the pseudo-token "#" have no other
   action.

   It is called after conflicts have been explained and before code
   generation takes place. The automaton is modified in place. *)

val default_conflict_resolution: unit -> unit

