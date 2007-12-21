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

open IL
open CodeBits

(* ------------------------------------------------------------------------ *)

(* A difficulty is that the functions whose code we wish to share are not
   identical -- they are usually identical up to the name of a concrete
   state, say, [MenhirState33] or [MenhirState47]. So, in order to detect
   sharing opportunities, we must abstract that concrete state away. *)

(* [abstract] attempts to abstract away one nullary data constructor,
   standing for such a state, in an expression. It returns a triple of
   the state that was abstracted away, a list of patterns that will
   later be used to eta-expand the expression, and the transformed
   expression (which has one extra parameter). [abstract] succeeds
   only if the code refers to exactly one concrete state; otherwise,
   it fails and raises [CannotAbstract]. *)

exception CannotAbstract

let abstract (code : expr) : string * pattern list * expr =

  (* This cell holds the data constructor that we have found and are
     going to abstract away. *)

  let data =
    ref None
  in

  (* The variable that we are going to replace it with. *)

  let var =
    prefix "s"
  in
  let evar =
    EVar var
  in
  let pvar =
    PVar var
  in

  (* An auxiliary function that attempts a replacement. *)

  let bingo d =
    match !data with
    | None ->
	data := Some d;
	evar
    | Some d' ->
	if d = d' then
	  evar
	else
	  raise CannotAbstract (* will not abstract away more than one data constructor *)
  in

  (* The traversal. *)

  let o = object (self)

    inherit Traverse.map as super

    (* Attempt to abstract away every constant data constructor that
       represents a state. *)

    method edata d es =
      if (es = []) && (Code.is_statecon d) then
	bingo d
      else
	super#edata d es

    (* Remove comments, because they might otherwise cause comparison
       between expressions to fail. (The code generator can produce
       identical code with distinct comments, because comments refer
       to states.) *)

    method ecomment _ e =
      self#expr e

    method epatcomment _ _ e =
      self#expr e

  end
  in

  (* An auxiliary function that builds a lambda-abstraction. If the
     code is already a lambda-abstraction (which it should be), then
     we merge the two abstractions and return a pair of the new code
     and the parameters of the existing abstraction. If there is a
     type annotation, we lift it above the new lambda. *)

  let lambda (code : expr) : pattern list * expr =
    match code with
    | EFun (formals, body) ->
	formals, EFun (pvar :: formals, body)
    | EAnnot (EFun (formals, body), scheme) ->
	formals, EAnnot (
	  EFun (pvar :: formals, body),
	  { scheme with body = arrow Code.tstate scheme.body }
	)
    | _ ->
	(* This case should not happen. *)
	[], EFun ([ pvar ], code)
  in

  (* Transform the code, wrap within a new lambda-abstraction, and
     return the name of the state that was abstracted away. *)

  let params, code = lambda (o#expr code) in
  match !data with
  | None ->
      (* Nothing was abstracted away. *)
      raise CannotAbstract
  | Some d ->
      (* A state named [d] was abstracted away. *)
      d, params, code

(* ------------------------------------------------------------------------ *)

(* [store table x y] adds a new binding of [x] to [y], in a hash table
   where keys are mapped to lists of data. *)

let store (table : ('a, 'b list) Hashtbl.t) (x : 'a) (y : 'b) =
  try
    let ys = Hashtbl.find table x in
    Hashtbl.replace table x (y :: ys)
  with Not_found ->
    Hashtbl.add table x [ y ]

(* ------------------------------------------------------------------------ *)

(* This auxiliary function returns the name of a value definition. *)

let nameof valdef =
  match valdef.valpat with
  | PVar x ->
      x
  | _ ->
      assert false

(* This auxiliary function creates a new name out of a nonempty list of
   existing names. This is done by sharing a greatest common prefix and
   concatenating the suffixes. *)

let fuse (names : string list) : string =
  let prefix = Misc.gcps names in
  let n = String.length prefix in
  let suffixes = List.map (fun name ->
    String.sub name n (String.length name - n)
  ) names in
  String.concat "_" (prefix :: suffixes)

(* ------------------------------------------------------------------------ *)

(* The bulk of the transformation code. *)

let share (program : program) : program =

  (* This table maps abstracted expressions to lists of definitions; it allows
     efficiently detecting when two definitions yield the same code after
     abstraction. Note that this technique relies on the generic hashing and
     equality functions, applied to expressions: this is somewhat
     brittle. More details below. *)

  let table : (expr, (string * pattern list * valdef) list) Hashtbl.t =
    Hashtbl.create 1023

  (* This counts the number of function definitions that are saved
     by this optimization. *)

  and saved =
    ref 0

  (* This accumulates the transformed definitions. *)

  and output : valdef list ref =
    ref []

  in

  (* First, examine every toplevel value definition. Attempt to abstract
     away a concrete state. If this succeeds, store the definition in the
     table, using the abstracted version of the code as the key. Otherwise,
     send this definition, unmodified, directly to the output stream. *)

  List.iter (fun (def : valdef) ->
    try
      let d, params, expr = abstract def.valval in
      store table expr (d, params, def)
    with CannotAbstract ->
      output := def :: !output
  ) program.valdefs;

  (* Next, process the contents of the hash table. We process one group
     at a time, where a group is a set of functions that have he same
     code up to abstraction of a state. *)

  Hashtbl.iter (fun (code : expr) (group : (string * pattern list * valdef) list) ->
    match group with

    | [] ->
	assert false (* cannot happen *)

    | [ (_, _, def) ] ->

	(* This is a singleton group. No sharing is possible. *)

	output := def :: !output

    | _ :: _ :: _ ->

	(* This is a group of at least two elements. *)

	(* Pick a name for the shared code. *)

	let f : string =
	  fuse (List.map (fun (_, _, def) -> nameof def) group)
	in
	
	(* Define the shared function. *)

	let fdef : valdef = {
	  valpublic = false;
	  valpat = PVar f;
	  valval = code;
	} in

	(* Re-define each of the original functions as a call to the shared
	   function, with an appropriate constant state as argument. Use
	   eta-expansion with respect to [params] so as to make sure that the
	   right-hand side is not syntactically a function application (which
	   would not be a legal let rec right hand side). *)

	let pat2expr = function
	  | PVar v ->
	      EVar v
	  | _ ->
	      assert false (* not implemented; should not be needed? *)
	in

	let defs : valdef list =
	  List.map (fun (d, params, def) ->
            { def with
	      valval =
		EFun (params,
		      EApp (EVar f, 
			    (EData (d, [])) :: (List.map pat2expr params)
			   )
		     )
	    }
	  ) group
	in

	(* Emit these definitions. *)

	output := fdef :: defs @ !output;

	(* Count how many functions are shared. Log which functions are shared. *)

	saved := !saved + List.length defs - 1;
	Error.logC 2 (fun f ->
	  Printf.fprintf f "Sharing: ";
	  List.iter (fun def -> Printf.fprintf f "%s " (nameof def)) defs;
	  Printf.fprintf f "\n"
	)

  ) table;

  (* Log how many functions are shared. *)

  Error.logC 1 (fun f ->
    Printf.fprintf f
      "Code sharing eliminated %d out of %d functions.\n"
      !saved
      (List.length program.valdefs)
  );

  (* Measure time. *)

  Time.tick "Code sharing";

  (* Done. *)

  { program with valdefs = List.rev !output }

(* ------------------------------------------------------------------------ *)

(* The external entry point. *)

let share p =
  if Settings.code_sharing then share p else p

