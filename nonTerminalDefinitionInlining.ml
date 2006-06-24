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

(* $Id: nonTerminalDefinitionInlining.ml,v 1.12 2006/06/12 12:33:14 regisgia Exp $ *)
open UnparameterizedSyntax
open ListMonad

(* This exception will be raised when a branch does not need inlining. *)
exception NoInlining

(* Color are used to detect cycles. *)
type 'a color = 
  | BeingExpanded
  | Expanded of 'a

(* Inline a grammar. The resulting grammar does not contain any definitions
   that can be inlined. *)
let inline grammar = 

  let use_inline = ref false in

  (* This table associates a color to each non terminal that can be expanded. *)
  let expanded_non_terminals = 
    Hashtbl.create 13 
  in

  let expanded_state k = 
    Hashtbl.find expanded_non_terminals k 
  in
      
  let mark_as_being_expanded k = 
    Hashtbl.add expanded_non_terminals k BeingExpanded
  in

  let mark_as_expanded k r =
    Hashtbl.replace expanded_non_terminals  k (Expanded r);
    r
  in

  (* This function traverses the producers of the branch [b] and find
     the first non terminal that can be inlined. If it finds one, it
     inlines its branches into [b], that's why this function can return
     several branches. If it does not find one non terminal to be 
     inlined, it raises [NoInlining]. *)
  let rec find_inline_producer b = 
    let prefix, nt, p, psym, suffix = 
      let rec chop_inline i (prefix, suffix) =
	match suffix with
	  | [] -> 
	      raise NoInlining

	  | ((nt, id) as x) :: xs ->
	      try
		let r = StringMap.find nt grammar.rules in
		let id = match id with
		  | None -> "_"^string_of_int i
		  | Some id -> id
		in
		  if r.inline_flag then 
		    (* We have to inline the rule [r] into [b] between
		       [prefix] and [xs]. *)
		    List.rev prefix, nt, r, id, xs
		  else 
		    chop_inline (i + 1) (x :: prefix, xs) 
	      with Not_found -> 
		chop_inline (i + 1) (x :: prefix, xs) 
      in
	chop_inline 1 ([], b.producers)
    in
      prefix, expand_rule nt p, nt, psym, suffix

  (* We have to rename producers' names of the inlined production 
     if they clashes with the producers' names of the branch into 
     which we do the inlining. *)
  and rename_if_necessary b producers =

    (* First we compute the set of names already in use. *)
    let producers_names = 
      List.fold_left (fun s -> function (_, Some x) -> StringSet.add x s | _ -> s) 
	StringSet.empty (b.producers @ producers)
    in

    (* This function returns a fresh name beginning with [prefix] and 
       that is not in the set of names [names]. *)
    let rec fresh ?(c=0) names prefix =
      let name = prefix^string_of_int c in
	if StringSet.mem name names then
	  fresh ~c:(c+1) names prefix
	else 
	  name
    in

    (* Compute a renaming and the new inlined producers' names. *)
    let phi, producers' =
      List.fold_left (fun (phi, producers) -> function (p, Some x) -> 
		if StringSet.mem x producers_names then
		  let x' = fresh producers_names x in
		    ((x, x') :: phi, (p, Some x') :: producers)
		else 
		  (phi, (p, Some x) :: producers)
		| p -> phi, p :: producers) ([], []) producers
    in
      phi, List.rev producers'
	
  (* Inline the non terminals that can be inlined in [b]. We use the 
     ListMonad to combine the results. *)
  and expand_branch (b : branch) : branch ListMonad.m =
    try 
      let prefix, p, nt, psym, suffix = find_inline_producer b in
	use_inline := true;
	if Action.use_dollar b.action then
	  Error.errorN [ b.branch_position ]
	    (Printf.sprintf 
	       "You cannot use %s and the $i syntax in this branch since the \
               definition of %s has to be inlined."
	       nt nt)
	else 
	  let inline_branch pb = 
	    let phi, inlined_producers = rename_if_necessary b pb.producers in
	      { b with
		  producers = prefix @ inlined_producers @ suffix;
		  action = 
		  Action.compose psym (Action.rename phi pb.action) b.action;
	      }
	  in
	    List.map inline_branch p.branches >>= expand_branch 
	  
    with NoInlining -> 
      return b
	
  (* Expand a rule if necessary. *)
  and expand_rule k r = 
    try 
      (match expanded_state k with
	 | BeingExpanded ->
    	     Error.errorN
	       r.positions
	       (Printf.sprintf "there is a cycle in the definition of %s." k)
	 | Expanded r ->
	     r)
    with Not_found ->
      mark_as_being_expanded k;
      mark_as_expanded k { r with branches = r.branches >>= expand_branch }
  in

    (* We check that the %inline rules do not use $i syntax since 
       expansion of $i is impossible. *)
  let _ = 
    StringMap.iter 
      (fun _ r -> 
	 if r.inline_flag 
	   && List.exists (fun b -> Action.use_dollar b.action) r.branches then
	     Error.errorN r.positions 
	       (Printf.sprintf 
		  "You cannot use $i syntax in this branch since its \
                   definition will be inlined."))
      grammar.rules
  in

    (* To expand a grammar, we expand all its rules and remove 
       the %inline rules. *)
  let expanded_rules = 
    StringMap.mapi expand_rule grammar.rules
  and useful_types = 
      StringMap.filter 
	(fun k _ -> try not (StringMap.find k grammar.rules).inline_flag
	 with Not_found -> true)
	grammar.types
  in

    { grammar with 
	rules = StringMap.filter (fun _ r -> not r.inline_flag) expanded_rules;
	types = useful_types
    }, !use_inline
      
