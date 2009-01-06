(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

(* This module discovers information about the shape of the stack
   in each of the automaton's states.

   It would probably be possible to predict this information, instead
   of discovering it, but it would then be less obvious that it is
   correct. In this approach, it is more obvious that the discovered
   invariant is correct. The drawback is that it is not necessarily
   clear why it is strong enough to guarantee that the generated code
   is well-typed. *)

open Grammar

(* ------------------------------------------------------------------------ *)
(* Discover what is known of the structure of the stack in every
   automaton node. This knowledge can be defined, to a first
   approximation, as the greatest common suffix of all paths that lead
   up to this node, where a path is viewed as a string of symbols.

   At each state, we distinguish two cases: either the stack is known
   entirely, or only a suffix of it is known. In other words, the part
   of the stack that is not described by the string of symbols is
   empty in the former case and unknown in the latter.

   We compute this information by beginning with an upper bound,
   namely the shortest path towards each node. The string that we are
   looking for is a substring of that path. Furthermore, we enforce
   the property that, when two paths join, they should agree about the
   structure of the stack at the join point. That is, whenever we find
   a new edge that leads to a known state, we take the greatest common
   suffix of the two strings that they correspond to.

   In addition to this information, we compute the set of states that
   are liable to appear in each stack cell. When two paths join, we
   take the union of the state sets that appear along the greatest
   common suffix. *)

type tail =
  | TailEmpty
  | TailUnknown

type cell =
    Symbol.t * Lr1.NodeSet.t

type word =
    cell list

type stack =
    word * tail

(* ------------------------------------------------------------------------ *)
(* This is how we extend a stack with a new symbol. *)

let extend cell (w, tail) =
  cell :: w, tail

(* ------------------------------------------------------------------------ *)
(* This is how we compute the meet of two stacks. *)

let rec tmeet (tail1 : tail) (tail2 : tail) : tail =
  match tail1, tail2 with
  | TailEmpty, TailEmpty ->
      TailEmpty
  | TailUnknown, _
  | _, TailUnknown ->
      TailUnknown

let rec wmeet (w1 : word) (w2 : word) : word * tail =
  match w1, w2 with
  | [], [] ->
      (* Both stacks are empty. There is agreement. *)
      [], TailEmpty
  | [], _ :: _
  | _ :: _, [] ->
      (* One stack is empty, but the other isn't. The greatest
	 common suffix is empty, and there is disagreement. *)
      [], TailUnknown
  | (symbol1, states1) :: w1, (symbol2, states2) :: w2 ->
      if Symbol.equal symbol1 symbol2 then
	(* The stacks agree on their top cell. It is therefore part
	   of the greatest common suffix. *)
        let w, tail = wmeet w1 w2 in
	(symbol1, Lr1.NodeSet.union states1 states2) :: w, tail
      else
        (* The stacks disagree on their top cell. Their greatest common
	   suffix is therefore empty. *)
	[], TailUnknown

let rec meet (stk1 : stack) (stk2 : stack) : stack =
  let w1, tail1 = stk1
  and w2, tail2 = stk2 in
  let w, tail = wmeet w1 w2 in
  w, tmeet tail (tmeet tail1 tail2)

(* ------------------------------------------------------------------------ *)
(* This is how we truncate a stack at a certain depth. *)

let truncate depth (w, tail) =
  assert (List.length w >= depth);
  Misc.truncate depth w, TailUnknown

(* ------------------------------------------------------------------------ *)
(* This is our initial estimation of the stack at every node. It will remain
   unmodified at entry nodes, but will be overwritten without consideration
   at all other nodes. *)

let empty : stack =
  [], TailEmpty

(* ------------------------------------------------------------------------ *)
(* This is a mapping of automaton nodes to stacks. *)

let stacks : stack array =
  Array.make Lr1.n empty

let stack node =
  stacks.(Lr1.number node)

let set_stack node h =
  stacks.(Lr1.number node) <- h

(* ------------------------------------------------------------------------ *)
(* Here comes the initial discovery phase. *)

let () =
  Lr1.bfs (fun discovery source symbol target ->

    (* There is a transition from [source] to [target] labeled
       [symbol]. This yields a new description of the stack at the
       target node, where the top cell holds [symbol] and [source],
       and where the structure of the remainder of the stack is the
       structure of the stack at the [source] node. *)

    let stk = extend (symbol, Lr1.NodeSet.singleton source) (stack source) in

    (* Define or update the structure of the stack at the [target]
       node, depending on whether this node was newly discovered. *)

    set_stack target (if discovery then stk else meet (stack target) stk)

  )

(* ------------------------------------------------------------------------ *)
(* We now discover what can be said of the structure of the stack when
   production [prod] is about to be reduced. At the same time, we
   count how many states can reduce each production and warn about
   productions that are never reduced. *)

type info =
  (* Production is never reduced. *)
  | Zero
  (* Production can be reduced at certain nodes with a certain stack structure. *)
  | More of Lr1.NodeSet.t * stack

type prodinfo =
    info ProductionMap.t

let find prod prodinfo =
  try
    ProductionMap.lookup prod prodinfo
  with Not_found ->
    Zero

let prodinfo : prodinfo =
  Lr1.fold (fun prodinfo node ->
    TerminalMap.fold (fun _ prods prodinfo ->
      let prod = Misc.single prods in
      ProductionMap.add prod (
	match find prod prodinfo with
	| Zero ->
	    More (
	      Lr1.NodeSet.singleton node,
	      truncate (Production.length prod) (stack node)
            )
	| More (nodes, stk') ->
	    More (
	      Lr1.NodeSet.add node nodes,
	      meet (stack node) stk'
            )
      ) prodinfo
    ) (Lr1.reductions node) prodinfo
  ) ProductionMap.empty

let () =
  let count = ref 0 in
  Production.iter (fun prod ->
    match find prod prodinfo, Production.classify prod with
    | Zero, Some nt ->
	incr count;
	Error.warningN
	  (Nonterminal.positions nt)
	  (Printf.sprintf "symbol %s is never accepted." (Nonterminal.print false nt))
    | Zero, None ->
	incr count;
	Error.warningN
	  (Production.positions prod)
	  (Printf.sprintf "production %sis never reduced." (Production.print prod))
    | More (_, (w, _)), _ ->
	assert (List.length w = Production.length prod)
  );
  if !count > 0 then
    Error.warning
      (Printf.sprintf "in total, %d productions are never reduced." !count)

let prodstack prod =
  match find prod prodinfo with
  | Zero ->
      assert false
  | More (_, stk) ->
      stk

(* ------------------------------------------------------------------------ *)
(* We now determine which states must be represented, that is,
   explicitly pushed onto the stack. For simplicity, a state is either
   always represented or never represented. More fine-grained
   strategies, where a single state is sometimes pushed onto the stack
   and sometimes not pushed, depending on which outgoing transition is
   being taken, are conceivable, but quite tricky, and probably not
   worth the trouble.

   (1) If two states are liable to appear within a single stack cell,
   then one is represented if and only if the other is
   represented. This ensures that the structure of stacks is known
   everywhere and that we can propose types for stacks.

   (2) If a state [s] has an outgoing transition along nonterminal
   symbol [nt], and if the [goto] table for symbol [nt] has more than
   one target, then state [s] is represented.

   (3) If a stack cell contains more than one state and if at least
   one if these states is able to handle the [error] token, then these
   states are represented.

   (4) If the semantic action associated with a production mentions
   the [$syntaxerror] keyword, then the state that is being reduced to
   (that is, the state that initiated the recognition of this
   production) is represented. (Indeed, it will be passed as an
   argument to [errorcase].) *)

(* Data. *)

let rep : bool UnionFind.point array =
  Array.init Lr1.n (fun _ -> UnionFind.fresh false)

(* Getter. *)

let represented state =
  rep.(Lr1.number state)

(* Setters. *)

let represent state =
  UnionFind.change (represented state) true

let represents states =
  represent (Lr1.NodeSet.choose states)

(* Enforce condition (1) above. *)

let share (w, _) =
  List.iter (fun (_, states) ->
    let dummy = UnionFind.fresh false in
    Lr1.NodeSet.iter (fun state ->
      UnionFind.eunion dummy (represented state)
    ) states
  ) w

let () =
  Array.iter share stacks;
  Production.iter (fun prod ->
    match find prod prodinfo with
    | Zero ->
	()
    | More (_, stk) ->
	share stk
  )

(* Enforce condition (2) above. *)

let () =
  Nonterminal.iter (fun nt ->
    let count = 
      Lr1.targets (fun count _ _ ->
	count + 1
      ) 0 (Symbol.N nt)
    in
    if count > 1 then
      Lr1.targets (fun () sources _ ->
	List.iter represent sources
      ) () (Symbol.N nt)
  )

(* Enforce condition (3) above. *)

let handler state =
  try
    let _ = SymbolMap.find (Symbol.T Terminal.error) (Lr1.transitions state) in
    true
  with Not_found ->
    try
      let _ = TerminalMap.lookup Terminal.error (Lr1.reductions state) in
      true
    with Not_found ->
      false

let handlers states =
  Lr1.NodeSet.exists handler states

let () =
  Array.iter (fun (w, _) ->
    List.iter (fun (_, states) ->
      if Lr1.NodeSet.cardinal states >= 2 && handlers states then
	represents states
    ) w
  ) stacks

(* Enforce condition (4) above. *)

let () =
  Production.iterx (fun prod ->
    if Action.has_syntaxerror (Production.action prod) then
      match find prod prodinfo with
      | Zero ->
	  ()
      | More (sites, (w, _)) ->
	  let length = Production.length prod in
	  if length = 0 then
	    Lr1.NodeSet.iter represent sites
	  else
	    let (_, states) = List.nth w (length - 1) in
	    represents states
  )

(* Define accessors. *)

let represented state =
  UnionFind.find (represented state)

let representeds states =
  if Lr1.NodeSet.is_empty states then
    assert false
  else
    represented (Lr1.NodeSet.choose states)

let representedc (_, states) =
  representeds states

let handlerc (_, states) =
  handlers states

let fold f accu w =
  List.fold_right (fun (symbol, states) accu ->
    f accu (representeds states) symbol
  ) w accu

let fold_top f accu w =
  match w with
  | [] ->
      accu
  | (symbol, states) :: _ ->
      f (representeds states) symbol

let () =
  Error.logC 1 (fun f ->
    let count =
      Lr1.fold (fun count node ->
        if represented node then count + 1 else count
      ) 0
    in
    Printf.fprintf f "%d out of %d states are represented.\n" count Lr1.n
  )

(* ------------------------------------------------------------------------ *)
(* Explain how the stack should be deconstructed when an error is
   found.

   We sometimes have a choice as too how many stack cells should be
   popped. Indeed, several cells in the known suffix of the stack may
   physically hold a state. If neither of these states handles errors,
   then we could jump to either. (Indeed, if we jump to one that's
   nearer, it will in turn pop further stack cells and jump to one
   that's farther.) In the interests of code size, we should pop as
   few stack cells as possible. So, we jump to the topmost represented
   state in the known suffix. *)

type state =
  | Represented
  | UnRepresented of Lr1.node

type instruction =
  | Die
  | DownTo of word * state

let rewind node : instruction =
  let w, tail = stack node in

  let rec rewind w =
    match w, tail with
    | [], TailEmpty ->
	Die
    | [], TailUnknown ->

	(* I believe that every stack description either is definite
	   (that is, ends with [TailEmpty]) or contains at least one
	   represented state. This property, if true, ensures that
	   this assertion cannot fail. *)

        (* TEMPORARY prove this property. If the property is not true in
           general, one could make it true by making more states
           represented. *)

	assert false

    | cell :: w, _ ->

	if representedc cell then

	  (* Here is a represented state. We will pop this
	     cell and no more. *)

	  DownTo ([ cell ], Represented)

	else if handlerc cell then

	  (* Here is an unrepresented state that can handle
	     errors. The cell must hold a singleton set of states, so
	     we know which state to jump to, even though it isn't
	     represented. *)

	  let (_, states) = cell in
	  assert (Lr1.NodeSet.cardinal states = 1);
	  let state = Lr1.NodeSet.choose states in
	  DownTo ([ cell ], UnRepresented state)

	else

	  (* Here an unrepresented state that does not handle
	     errors. Pop this cell and look further. *)

	  match rewind w with
	  | Die ->
	      Die
	  | DownTo (w, st) ->
	      DownTo (cell :: w, st)

  in
  rewind w

(* ------------------------------------------------------------------------ *)
(* Accessors for information about the stack. *)

let stack node : word =
  let (w, _) = stack node in
  w

let prodstack prod : word =
  let (w, _) = prodstack prod in
  w

let gotostack : Nonterminal.t -> word =
  Nonterminal.tabulate (fun nt ->
    let sources =
      Lr1.targets (fun accu sources _ ->
	List.fold_right Lr1.NodeSet.add sources accu
      ) Lr1.NodeSet.empty (Symbol.N nt)
    in
    [ Symbol.N nt, sources ]
  )

(* ------------------------------------------------------------------------ *)
(* We now determine which positions must be kept track of. For
   simplicity, we do this on a per symbol basis. That is, for each
   symbol, either we never keep track of position information, or we
   always do. In fact, we do distinguish start and end positions.
   This leads to computing two sets of symbols -- those that keep
   track of their start position and those that keep track of their
   end position.

   A symbol on the right-hand side of a production must keep track of
   its (start or end) position if that position is explicitly
   requested by a semantic action.

   Furthermore, if the left-hand symbol of a production must keep
   track of its start (resp. end) position, then the first
   (resp. last) symbol of its right-hand side (if there is one) must
   do so as well. That is, unless the right-hand side is empty. *)

open Keyword

let startp =
  ref SymbolSet.empty

let endp =
  ref SymbolSet.empty

let rec require where symbol =
  let wherep =
    match where with
    | WhereStart ->
	startp
    | WhereEnd ->
	endp
  in
  if not (SymbolSet.mem symbol !wherep) then begin
    wherep := SymbolSet.add symbol !wherep;
    match symbol with
    | Symbol.T _ ->
	()
    | Symbol.N nt ->
	Production.iternt nt (require_aux where)
  end

and require_aux where prod =
  let nt, rhs = Production.def prod in
  let length = Array.length rhs in
  if length > 0 then
    match where with
    | WhereStart ->
	require where rhs.(0)
    | WhereEnd ->
	require where rhs.(length - 1)

let () =
  Production.iterx (fun prod ->
    let rhs = Production.rhs prod
    and ids = Production.identifiers prod
    and action = Production.action prod in

    KeywordSet.iter (function
      | Dollar _
      | PreviousError
      | SyntaxError ->
	  ()
      | Position (Left, where, _) ->
	  require_aux where prod
      | Position (RightDollar i, where, _) ->
	  require where rhs.(i - 1)
      | Position (RightNamed id, where, _) ->
	  Array.iteri (fun i id' ->
	    if id = id' then
	      require where rhs.(i)
	  ) ids
    ) (Action.keywords action)
  )

let startp =
  !startp

let endp =
  !endp

let () =
  Error.logC 1 (fun f ->
    Printf.fprintf f
      "%d out of %d symbols keep track of their start position.\n\
       %d out of %d symbols keep track of their end position.\n"
        (SymbolSet.cardinal startp) (Terminal.n + Nonterminal.n)
        (SymbolSet.cardinal endp) (Terminal.n + Nonterminal.n))

let startp symbol =
  SymbolSet.mem symbol startp

let endp symbol =
  SymbolSet.mem symbol endp

(* ------------------------------------------------------------------------ *)
(* Information about which productions are reduced and where. *)

let ever_reduced prod =
   match find prod prodinfo with
   | Zero ->
       false
   | More _ ->
       true

let fold_reduced f prod accu =
  match find prod prodinfo with
  | Zero ->
      accu
  | More (nodes, _) ->
      Lr1.NodeSet.fold f nodes accu

(* ------------------------------------------------------------------------- *)
(* Miscellaneous. *)

let universal symbol =
  Lr1.fold (fun universal s ->
    universal && (if represented s then SymbolMap.mem symbol (Lr1.transitions s) else true)
  ) true

(* ------------------------------------------------------------------------ *)
(* Discover which states potentially can do error recovery.

   They are the states whose incoming symbol is [error]. At these
   states, [env.shifted] is zero, that is, no tokens have been
   successfully shifted since the last error token was shifted.

   We do not include in this definition the states where [env.shifted]
   *may be* zero. That would involve adding in all states reachable
   from the above states via reductions. However, error recovery will
   never be performed in these states. Indeed, imagine we shift an
   error token and enter a state that can do error recovery, according
   to the above definition. If, at this point, we consult the
   lookahead token [tok] and perform a reduction, then the new state
   that we reach is, by construction, able to act upon [tok], so no
   error recovery will be performed at that state, even though
   [env.shifted] is still zero. However, we must not perform default
   reductions at states that can do error recovery, otherwise we break
   this reasoning.

   If the option [--error-recovery] was not provided on the command
   line, then no states will perform error recovery. This makes things
   simpler (and saves some code) in the common case where people are
   not interested in error recovery. This also disables the warning
   about states that can do error recovery but do not accept the EOF
   token. *)

let recoverers =
  if Settings.recovery then
    Lr1.fold (fun recoverers node ->
      match Lr1.incoming_symbol node with
      | Some (Symbol.T tok)
	when Terminal.equal tok Terminal.error ->
	  Lr1.NodeSet.add node recoverers
      | _ ->
	  recoverers
    ) Lr1.NodeSet.empty
  else
    Lr1.NodeSet.empty

let recoverer node =
  Lr1.NodeSet.mem node recoverers

(* ------------------------------------------------------------------------ *)
(* Discover which states can peek at an error. These are the states
   where [env.shifted] may be -1, that is, where an error token may be
   on the stream. These are all states that are targets of a reduce
   action on [error]. *)

let errorpeekers =
  Lr1.fold (fun errorpeekers node ->
    try
      let prods = TerminalMap.lookup Terminal.error (Lr1.reductions node) in
      let prod = Misc.single prods in
      let nt = Production.nt prod in
      Lr1.targets (fun errorpeekers _ target ->
	Lr1.NodeSet.add target errorpeekers
      ) errorpeekers (Symbol.N nt)
    with Not_found ->
      errorpeekers
  ) Lr1.NodeSet.empty

let errorpeeker node =
  Lr1.NodeSet.mem node errorpeekers

(* ------------------------------------------------------------------------ *)
(* Here is how we check whether state [s] should have a default
   reduction.

   We check whether [s] has no outgoing shift transitions and only has
   one possible reduction action. In that case, we produce a default
   reduction action, that is, we perform reduction without consulting
   the lookahead token. This saves code, but can alter the parser's
   behavior in the presence of errors.

   A state that can perform error recovery (that is, a state whose
   incoming symbol is [error]) never performs a default
   reduction. This is explained above. Actually, we allow one
   exception: if the state has a single (reduction) action on "#", as
   explained in the next paragraph, then we perform this default
   reduction and do not allow error recovery to take place. Error
   recovery would not make much sense, since we believe we are at the
   end of file.

   The check for default actions subsumes the check for the case where
   [s] admits a reduce action with lookahead symbol "#". In that case,
   it must be the only possible action -- see
   [Lr1.default_conflict_resolution]. That is, we have reached a point
   where we have recognized a well-formed input and are now expecting
   an end-of-stream. In that case, performing reduction without
   looking at the next token is the right thing to do, since there
   should in fact be none. The state that we reduce to will also have
   the same property, and so on, so we will in fact end up rewinding
   the entire stack and accepting the input when the stack becomes
   empty. *)

let (has_default_reduction : Lr1.node -> (Production.index * TerminalSet.t) option), hdrcount =
  Misc.tabulateo Lr1.number Lr1.fold Lr1.n (fun s ->

    match ProductionMap.is_singleton (Lr1.invert (Lr1.reductions s)) with
    | Some (_, toks)  as reduction
      when SymbolMap.purelynonterminal (Lr1.transitions s) ->
    
	if TerminalSet.mem Terminal.sharp toks then
	  (* Perform default reduction on "#". *)
	  reduction
	else if recoverer s then
	  (* Do not perform default reduction. Allow error recovery. *)
	  None
	else
	  (* Perform default reduction. *)
	  reduction

    | Some _
    | None ->
	None

  )

let () =
  Error.logC 1 (fun f ->
    Printf.fprintf f
       "%d out of %d states have a default reduction.\n"
       hdrcount Lr1.n)

(* ------------------------------------------------------------------------ *)

let () =
  Time.tick "Constructing the invariant"

