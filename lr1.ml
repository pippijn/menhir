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

(* ------------------------------------------------------------------------ *)
(* Nodes. *)

type node = {

    (* Nodes are numbered. *)

    mutable number: int;

    (* Each node is associated with a state. This state can change
       during construction as nodes are merged. *)

    mutable state: Lr0.lr1state;

    (* Each node carries information about its outgoing transitions
       and about its reductions. *)

    mutable transitions: node SymbolMap.t;
    mutable reductions: Production.index list TerminalMap.t;

    (* Tokens for which there are several possible behaviors are
       conflict tokens. *)

    mutable conflict_tokens: TerminalSet.t;

    (* Transitions are also stored in reverse, so as to allow reverse
       traversals of the automaton. *)

    mutable predecessors: node list;

    (* If a node has any incoming transitions, then they all carry
       the same symbol. This is it. *)

    mutable incoming_symbol: Symbol.t option;

    (* Transient marks are used during construction and traversal. *)

    mutable mark: Mark.t;

  }

(* ------------------------------------------------------------------------ *)
(* Construction of the automaton. *)

(* A queue of pending nodes, whose successors should be explored or
   re-explored. *)

let pending =
  Mark.fresh()

let queue : node Queue.t =
  Queue.create()

let enqueue node =
  if not (Mark.same node.mark pending) then begin
    node.mark <- pending;
    Queue.add node queue
  end;
  node

(* A mapping of LR(0) node numbers to lists of nodes. This allows us to
   efficiently find all existing nodes that are core-compatible with a
   newly found state. *)

let map : node list array =
  Array.create Lr0.n []

(* Exploring a state; this returns a (new or existing) node
   and, if necessary, enqueues this node for exploration. *)

exception Subsumed of node
exception Compatible of node

let num =
  ref 0

let explore (state : Lr0.lr1state) : node =
  try

    (* Find all existing states that share the same core. *)

    let k = Lr0.core state in
    assert (k < Lr0.n);
    let similar = map.(k) in

    (* Check whether one of these states subsume the candidate
       new state. If so, no need to create a new state: just
       reuse the existing one. *)

    List.iter (fun node ->
      if Lr0.subsume state node.state then
	raise (Subsumed node)
    ) similar;

    (* Check whether one of the existing states is compatible,
       in Pager's sense, with the new state. If so, no need to
       create a new state: just merge the new state into the
       existing one. This requires reevaluating its successors. *)

    if Settings.pager then
      List.iter (fun node ->
	if Lr0.compatible state node.state &&
	   Lr0.eos_compatible state node.state then
	  raise (Compatible node)
      ) similar;

    (* Otherwise, create a new node. Two states that are in the
       subsumption relation are also compatible. This implies that the
       newly created node does not subsume any existing states. *)

    incr num;

    let node = {
      state = state;
      transitions = SymbolMap.empty;
      reductions = TerminalMap.empty;
      conflict_tokens = TerminalSet.empty;
      number = 0; (* temporary placeholder *)
      mark = Mark.none;
      predecessors = [];
      incoming_symbol = None;
    } in

    map.(k) <- node :: similar;

    enqueue node

  with

  | Compatible node ->
      node.state <- Lr0.union state node.state;
      enqueue node

  | Subsumed node ->
      node

(* Populate the queue with the start nodes and store them in an
   array. *)

let entry : node array =
  Array.map (fun (k : Lr0.node) ->
    explore (Lr0.start k)
  ) Lr0.entry

(* Until the queue is empty, take a node out the queue, construct the
   nodes that correspond to its successors, and enqueue them. *)

let () =
  Misc.qiter (fun node ->
    node.mark <- Mark.none;
    node.transitions <- SymbolMap.map explore (Lr0.transitions node.state)
  ) queue

let n =
  !num

let () =
  Error.logA 1 (fun f -> Printf.fprintf f "Built an LR(1) automaton with %d states.\n" !num)

(* ------------------------------------------------------------------------ *)
(* We now perform one depth-first traversal of the automaton,
   recording predecessor edges, numbering nodes, sorting nodes
   according to their incoming symbol, building reduction tables, and
   finding out which nodes have conflicts. *)

(* A count of all nodes. *)

let () =
  num := 0

(* A list of all nodes. *)

let nodes : node list ref =
  ref []

(* A list of nodes with conflicts. *)

let conflict_nodes : node list ref =
  ref []

(* Counts of nodes with shift/reduce and reduce/reduce conflicts. *)

let shift_reduce =
  ref 0

let reduce_reduce =
  ref 0

(* Count of the shift/reduce conflicts that could be silently
   resolved. *)

let silently_solved =
  ref 0

(* A mapping of symbols to lists of nodes that admit this incoming
   symbol. *)

let incoming : node list SymbolMap.t ref =
  ref SymbolMap.empty

(* Go ahead. *)

let () =

  let marked = Mark.fresh() in

  let rec visit node =
    if not (Mark.same node.mark marked) then begin
      node.mark <- marked;
      nodes := node :: !nodes;

      (* Number this node. *)

      let number = !num in
      num := number + 1;
      node.number <- number;

      (* Insertion of a new reduce action into the table of reductions. *)

      let addl prod tok reductions =
	let prods =
	  try
	    TerminalMap.lookup tok reductions
	  with Not_found ->
	    []
	in
	TerminalMap.add tok (prod :: prods) reductions
      in

      (* Build the reduction table. Here, we gather all potential
         reductions, without attempting to solve shift/reduce
         conflicts on the fly, because that would potentially hide
         shift/reduce/reduce conflicts, which we want to be aware
         of. *)

      let reductions =
	List.fold_left (fun reductions (toks, prod) ->
	  TerminalSet.fold (addl prod) toks reductions
        ) TerminalMap.empty (Lr0.reductions node.state)
      in

      (* Detect conflicts. Attempt to solve shift/reduce conflicts
	 when unambiguously allowed by priorities. *)

      let has_shift_reduce = ref false
      and has_reduce_reduce = ref false in

      node.reductions <-
	TerminalMap.fold (fun tok prods reductions ->
	  if SymbolMap.mem (Symbol.T tok) node.transitions then begin

	    (* There is a transition in addition to the reduction(s). We
	       have (at least) a shift/reduce conflict. *)

	    assert (not (Terminal.equal tok Terminal.sharp));
	    match prods with
	    | [] ->
		assert false
	    | [ prod ] ->
		begin

		  (* This is a single shift/reduce conflict. If priorities tell
		     us how to solve it, we follow that and modify the automaton. *)

		  match Precedence.shift_reduce tok prod with

		  | Precedence.ChooseShift ->

		      (* Suppress the reduce action. *)

		      incr silently_solved;
		      reductions

		  | Precedence.ChooseReduce ->

		      (* Record the reduce action and suppress the shift transition.
			 The automaton is modified in place. This can have the subtle
			 effect of making some nodes unreachable. Any conflicts in these
			 nodes will then be ignored (as they should be). *)

		      incr silently_solved;
		      node.transitions <- SymbolMap.remove (Symbol.T tok) node.transitions;
		      TerminalMap.add tok prods reductions

		  | Precedence.ChooseNeither ->

		      (* Suppress the reduce action and the shift transition. *)

		      incr silently_solved;
		      node.transitions <- SymbolMap.remove (Symbol.T tok) node.transitions;
		      reductions

		  | Precedence.DontKnow ->

		      (* Priorities don't allow concluding. Record the
			 existence of a shift/reduce conflict. *)

		      node.conflict_tokens <- Grammar.TerminalSet.add tok node.conflict_tokens;
		      has_shift_reduce := true;
		      TerminalMap.add tok prods reductions

		end

	    | prod1 :: prod2 :: _ ->

		(* This is a shift/reduce/reduce conflict. If the priorities
		   are such that each individual shift/reduce conflict is solved
		   in favor of shifting or in favor of neither, then solve the entire
		   composite conflict in the same way. Otherwise, report the conflict. *)

		let choices = List.map (Precedence.shift_reduce tok) prods in

		if List.for_all (fun choice ->
		  match choice with
		  | Precedence.ChooseShift -> true
		  | _ -> false
                ) choices then begin

		  (* Suppress the reduce action. *)

		  silently_solved := !silently_solved + List.length prods;
		  reductions

		end
		else if List.for_all (fun choice ->
		  match choice with
		  | Precedence.ChooseNeither -> true
		  | _ -> false
                ) choices then begin

		  (* Suppress the reduce action and the shift transition. *)

		  silently_solved := !silently_solved + List.length prods;
		  node.transitions <- SymbolMap.remove (Symbol.T tok) node.transitions;
		  reductions

		end
		else begin

		  (* Record a shift/reduce/reduce conflict. Keep all reductions. *)

		  node.conflict_tokens <- Grammar.TerminalSet.add tok node.conflict_tokens;
		  has_shift_reduce := true;
		  has_reduce_reduce := true;
		  TerminalMap.add tok prods reductions

		end

	  end
	  else
	    let () = 
	      match prods with
	      | []
	      | [ _ ] ->
		  ()
	      | prod1 :: prod2 :: _ ->

		  (* There is no transition in addition to the reduction(s). We
		     have a pure reduce/reduce conflict. Do nothing about it at
		     this point. *)

		  node.conflict_tokens <- Grammar.TerminalSet.add tok node.conflict_tokens;
		  has_reduce_reduce := true

	    in
	    TerminalMap.add tok prods reductions

      ) reductions TerminalMap.empty;

      (* Record statistics about conflicts. *)

      if not (TerminalSet.is_empty node.conflict_tokens) then begin
	conflict_nodes := node :: !conflict_nodes;
	if !has_shift_reduce then
	  incr shift_reduce;
	if !has_reduce_reduce then
	  incr reduce_reduce
      end;

      (* Continue the depth-first traversal. Record predecessors edges
         as we go. No ancestor appears twice in a list of
         predecessors, because two nodes cannot be related by two
         edges that carry distinct symbols. *)

      SymbolMap.iter (fun symbol son ->
        begin
	  match son.incoming_symbol with
	  | None ->
	      son.incoming_symbol <- Some symbol;
	      let others =
		try
		  SymbolMap.find symbol !incoming
		with Not_found ->
		  []
	      in
	      incoming := SymbolMap.add symbol (son :: others) !incoming
	  | Some symbol' ->
	      assert (Symbol.equal symbol symbol')
	end;
	son.predecessors <- node :: son.predecessors;
	visit son
      ) node.transitions
    end
  in
  
  Array.iter visit entry

let nodes =
  !nodes

let conflict_nodes =
  !conflict_nodes

let incoming =
  !incoming

let () =
  if !silently_solved = 1 then
    Error.logA 1 (fun f -> Printf.fprintf f "One shift/reduce conflict was silently solved.\n")
  else if !silently_solved > 1 then
    Error.logA 1 (fun f -> Printf.fprintf f "%d shift/reduce conflicts were silently solved.\n" !silently_solved);
  if !num < n then
    Error.logA 1 (fun f -> Printf.fprintf f "Only %d states remain after resolving shift/reduce conflicts.\n" !num)

let () =
  Grammar.diagnostics()

let n =
  !num

(* ------------------------------------------------------------------------ *)
(* Breadth-first iteration over all nodes. *)

let bfs =
  let module B = Breadth.Make (struct
    type vertex = node
    type label = Symbol.t
    let set_mark node m = node.mark <- m
    let get_mark node = node.mark
    let entry f = Array.iter f entry
    let successors f node = SymbolMap.iter f node.transitions
  end) in
  B.search

(* ------------------------------------------------------------------------ *)
(* Iteration over all nodes. *)

let fold f accu =
  List.fold_left f accu nodes

(* -------------------------------------------------------------------------- *)
(* Our output channel. *)

let out =
  lazy (open_out (Settings.base ^ ".automaton"))

(* ------------------------------------------------------------------------ *)
(* If requested, dump a verbose description of the automaton. *)

let () =
  Time.tick "Construction of the LR(1) automaton";
  if Settings.dump then begin
    fold (fun () node ->
      let out = Lazy.force out in
      Printf.fprintf out "State %d:\n%s" node.number (Lr0.print node.state);
      SymbolMap.iter (fun symbol node ->
	Printf.fprintf out "-- On %s shift to state %d\n"
	  (Symbol.print symbol) node.number
      ) node.transitions;
      TerminalMap.iter (fun tok prods ->
	List.iter (fun prod ->
	  (* TEMPORARY factoriser les symboles qui conduisent a reduire une meme production *)
	  Printf.fprintf out "-- On %s " (Terminal.print tok);
	  match Production.classify prod with
	  | Some nt ->
	      Printf.fprintf out "accept %s\n" (Nonterminal.print false nt)
	  | None ->
	      Printf.fprintf out "reduce production %s\n" (Production.print prod)
	) prods
      ) node.reductions;
      if not (TerminalSet.is_empty node.conflict_tokens) then
	Printf.fprintf out "** Conflict on %s\n" (TerminalSet.print node.conflict_tokens);
      Printf.fprintf out "\n%!"
    ) ();
    Time.tick "Dumping the LR(1) automaton"
  end

(* ------------------------------------------------------------------------ *)
(* [reverse_dfs goal] performs a reverse depth-first search through
   the automaton, starting at node [goal], and marking the nodes
   traversed. It returns a function that tells whether a node is
   marked, that is, whether a path leads from that node to the goal
   node. *)

let reverse_dfs goal =

  let mark = Mark.fresh() in

  let marked node =
    Mark.same node.mark mark
  in

  let rec visit node =
     if not (marked node) then begin
       node.mark <- mark;
       List.iter visit node.predecessors
     end
  in

  visit goal;
  marked

(* ------------------------------------------------------------------------ *)
(* Iterating over all nodes that are targets of edges carrying a
   certain symbol. The sources of the corresponding edges are also
   provided. *)

let targets f accu symbol =
  let targets =
    try
      SymbolMap.find symbol incoming
    with Not_found ->
      (* There are no incoming transitions on the start symbols. *)
      []
  in
  List.fold_left (fun accu target ->
    f accu target.predecessors target
  ) accu targets

(* ------------------------------------------------------------------------ *)
(* Converting a start node into the single item that it contains. *)

let start2item node =
  let state : Lr0.lr1state = node.state in
  let core : Lr0.node = Lr0.core state in
  let items : Item.Set.t = Lr0.items core in
  assert (Item.Set.cardinal items = 1);
  Item.Set.choose items

(* ------------------------------------------------------------------------ *)
(* Accessors. *)

let number node =
  node.number

let state node =
  node.state

let transitions node =
  node.transitions

let reductions node =
  node.reductions

let conflicts f =
  List.iter (fun node ->
    f node.conflict_tokens node
  ) conflict_nodes

let incoming_symbol node =
  node.incoming_symbol

(* ------------------------------------------------------------------------ *)
(* Report statistics. *)

let () =
  if !shift_reduce = 1 then
    Error.warning "one state has shift/reduce conflicts."
  else if !shift_reduce > 1 then
    Error.warning (Printf.sprintf "%d states have shift/reduce conflicts." !shift_reduce);
  if !reduce_reduce = 1 then
    Error.warning "one state has reduce/reduce conflicts."
  else if !reduce_reduce > 1 then
    Error.warning (Printf.sprintf "%d states have reduce/reduce conflicts." !reduce_reduce)

(* ------------------------------------------------------------------------ *)
(* When requested by the code generator, apply default conflict
   resolution to ensure that the automaton is deterministic. *)

(* [best prod prods] chooses which production should be reduced
   among the list [prod :: prods]. It fails if no best choice
   exists. *)

let rec best choice = function
  | [] ->
      choice
  | prod :: prods ->
      match Precedence.reduce_reduce choice prod with
      | Some choice ->
	  best choice prods
      | None ->
	  Error.signalN
	    (Production.positions choice @ Production.positions prod)
	    (Printf.sprintf
	       "will not resolve reduce/reduce conflict between\n\
                productions that originate in distinct source files:\n%s\n%s"
                  (Production.print choice)
                  (Production.print prod));
	  choice (* dummy *)

(* Go ahead. *)

let default_conflict_resolution () =

  let shift_reduce =
    ref 0
  and reduce_reduce =
    ref 0
  in

  List.iter (fun node ->

    node.reductions <-
      TerminalMap.fold (fun tok prods reductions ->
	try
	  let (_ : node) =
	    SymbolMap.find (Symbol.T tok) node.transitions
	  in
	  (* There is a transition at this symbol, so this
	     is a (possibly multiway) shift/reduce conflict.
	     Resolve in favor of shifting by suppressing all
	     reductions. *)
	  shift_reduce := List.length prods + !shift_reduce;
          reductions
	with Not_found ->
	  (* There is no transition at this symbol. Check
	     whether we have multiple reductions. *)
	  match prods with
	  | [] ->
	      assert false
	  | [ _ ] ->
	      TerminalMap.add tok prods reductions
	  | prod :: ((_ :: _) as prods) ->
	      (* We have a reduce/reduce conflict. Resolve, if
		 possible, in favor of a single reduction.
	         This reduction must be preferrable to each
	         of the others. *)
	      reduce_reduce := List.length prods + !reduce_reduce;
	      TerminalMap.add tok [ best prod prods ] reductions

      ) node.reductions TerminalMap.empty

  ) conflict_nodes;

  if !shift_reduce = 1 then
    Error.warning "one shift/reduce conflict was arbitrarily resolved."
  else if !shift_reduce > 1 then
    Error.warning (Printf.sprintf "%d shift/reduce conflicts were arbitrarily resolved." !shift_reduce);
  if !reduce_reduce = 1 then
    Error.warning "one reduce/reduce conflict was arbitrarily resolved."
  else if !reduce_reduce > 1 then
    Error.warning (Printf.sprintf "%d reduce/reduce conflicts were arbitrarily resolved." !reduce_reduce);

  (* Now, ensure that states that have a reduce action at the
     pseudo-token "#" have no other action. *)

  let ambiguities =
    ref 0
  in

  fold (fun () node ->
    
    try
      let prods, reductions = TerminalMap.lookup_and_remove Terminal.sharp node.reductions in
      let prod = Misc.single prods in

      (* This node has a reduce action at "#". Determine whether there
	 exist other actions. If there exist any other actions,
	 suppress this reduce action, and signal an ambiguity.

	 We signal an ambiguity even in the case where all actions at
	 this node call for reducing a single production. Indeed, in
	 that case, even though we know that this production must be
	 reduced, we do not know whether we should first discard the
	 current token (and call the lexer). *)

      let has_ambiguity = ref false in
      let toks = ref TerminalSet.empty in

      TerminalMap.iter (fun tok prods ->
	node.reductions <- reductions;
	has_ambiguity := true;
	toks := TerminalSet.add tok !toks
      ) reductions;

      SymbolMap.iter (fun symbol _ ->
	match symbol with
	| Symbol.N _ ->
	    ()
	| Symbol.T tok ->
	    node.reductions <- reductions;
	    has_ambiguity := true;
	    toks := TerminalSet.add tok !toks
      ) node.transitions;

      if !has_ambiguity then begin
	incr ambiguities;
	if Settings.dump then begin
	  Printf.fprintf (Lazy.force out)
	    "State %d has an end-of-stream conflict. There is a tension between\n\
	     (1) %s\n\
	     without even requesting a lookahead token, and\n\
	     (2) checking whether the lookahead token is %s%s,\n\
             which would require some other action.\n\n"
            (number node)
            (match Production.classify prod with
	    | Some nt ->
		Printf.sprintf "accepting %s" (Nonterminal.print false nt)
	    | None ->
		Printf.sprintf "reducing production %s" (Production.print prod))
            (if TerminalSet.cardinal !toks > 1 then "one of " else "")
            (TerminalSet.print !toks)
	end
      end

    with Not_found ->
      ()

  ) ();

  if !ambiguities = 1 then
    Error.warning "one state has an end-of-stream conflict."
  else if !ambiguities > 1 then
    Error.warning (Printf.sprintf "%d states have an end-of-stream conflict." !ambiguities);

  (* If any fatal error was signaled above, stop now. *)

  if Error.errors() then
    exit 1

