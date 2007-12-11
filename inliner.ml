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

let inline ({ valdefs = defs } as p : program) =

  (* Create a table of all global definitions. *)

  let before, table = Traverse.tabulate_defs defs in

  (* Count how many times each function is used, including inside its
     own definition. The public functions serve as starting points for
     this discovery phase. We rely on the simplifying assumption that
     no global definition is shadowed by a local definition. *)

  let queue : valdef Queue.t =
    Queue.create()
  and usage : int StringMap.t ref =
    ref StringMap.empty
  in

  let visit id =
    try
      let _, def = Hashtbl.find table id in

      (* This is a globally defined identifier. Increment its usage
	 count. If it was never visited, enqueue its definition for
         exploration. *)

      let n =
	try
	  StringMap.find id !usage
	with Not_found ->
	  Queue.add def queue;
	  0
      in
      usage := StringMap.add id (n + 1) !usage

    with Not_found ->
      (* Not a globally defined identifier. *)
      ()
  in

  (* Look for occurrences of identifiers inside expressions. *)

  let o =
    object
	inherit [ unit ] Traverse.fold
	method evar () id =
	  visit id
    end
  in

  (* Initialize the queue with all public definitions, and work from
     there. We assume that the left-hand side of every definition is
     a variable. *)

  List.iter (fun { valpublic = public; valpat = p } ->
    if public then
      visit (pat2var p)
  ) defs;
  Misc.qfold o#valdef () queue;
  let usage = !usage in

  (* Now, inline every function that is called at most once. At the
     same time, every function that is never called is dropped. The
     public functions again serve as starting points for the
     traversal. *)

  let queue : valdef Queue.t =
    Queue.create()
  and emitted =
    ref StringSet.empty
  in

  let enqueue def =
    let id = pat2var def.valpat in
    if not (StringSet.mem id !emitted) then begin
      emitted := StringSet.add id !emitted;
      Queue.add def queue
    end
  in

  (* A simple application is an application of a variable to a number
     of variables, constants, or record accesses out of variables. *)

  let rec is_simple_arg = function
    | EVar _
    | EData (_, [])
    | ERecordAccess (EVar _, _) ->
	true
    | EMagic e ->
	is_simple_arg e
    | _ ->
	false
  in

  let is_simple_app = function
    | EApp (EVar _, actuals) ->
	List.for_all is_simple_arg actuals
    | _ ->
	false
  in

  (* Taking a fresh instance of a type scheme. Ugly. *)

  let instance =
    let count = ref 0 in
    let fresh tv =
      incr count;
      tv, Printf.sprintf "freshtv%d" !count
    in
    fun scheme ->
      let mapping = List.map fresh scheme.quantifiers in
      let rec sub typ =
	match typ with
	| TypTextual _ ->
	    typ
	| TypVar v ->
	    begin try
	      TypVar (List.assoc v mapping)
	    with Not_found ->
	      typ
	    end
	| TypApp (f, typs) ->
	    TypApp (f, List.map sub typs)
	| TypTuple typs ->
	    TypTuple (List.map sub typs)
	| TypArrow (typ1, typ2) ->
	    TypArrow (sub typ1, sub typ2)
      in
      sub scheme.body
  in

  (* Destructuring a type annotation. *)

  let rec annotate formals body typ =
    match formals, typ with
    | [], _ ->
	[], EAnnot (body, type2scheme typ)
    | formal :: formals, TypArrow (targ, tres) ->
	let formals, body = annotate formals body tres in
	PAnnot (formal, targ) :: formals, body
    | _ :: _, _ ->
	(* Type annotation has insufficient arity. *)
	assert false
  in

  (* The heart of the inliner: rewriting a function call to a [let]
     expression. 

     If there was a type annotation at the function definition site,
     it is dropped, provided [--infer] was enabled. Otherwise, it is
     kept, because, due to the presence of [EMagic] expressions in the
     code, dropping a type annotation could cause an ill-typed program
     to become apparently well-typed. Keeping a type annotation
     requires taking a fresh instance of the type scheme, because
     OCaml doesn't have support for locally and existentially bound
     type variables. Yuck. *)

  let inline formals actuals body oscheme =
    assert (List.length actuals = List.length formals);
    match oscheme with
    | Some scheme
      when not Settings.infer ->

	let formals, body = annotate formals body (instance scheme) in
	mlet formals actuals body

    | _ ->
	mlet formals actuals body
  in

  (* Look for occurrences of identifiers inside expressions, branches,
     etc. and replace them with their definitions if they have only
     one use site or if their definitions are sufficiently simple. *)

  let o =
    object (self)
      inherit Traverse.map as super
      method eapp e actuals =
	match e with
	| EVar id ->
	    begin
	      try
		let _, def = Hashtbl.find table id in

		let formals, body, oscheme =
		  match def with
		  | { valval = EFun (formals, body) } ->
		      formals, body, None
		  | { valval = EAnnot (EFun (formals, body), scheme) } ->
		      formals, body, Some scheme
		  | { valval = _ } ->
		      (* Definition is not a function definition.
			 This should not happen in the kind of code that we
			 generate. *)
		      assert false
		in

		assert (StringMap.mem id usage);
		if StringMap.find id usage = 1 || is_simple_app body then

		  (* Definition can be inlined, with beta reduction. *)

		  inline formals (self#exprs actuals) (EComment (id, self#expr body)) oscheme

		else begin

		  (* Definition cannot be inlined. *)
		  enqueue def;
		  EApp (EVar id, self#exprs actuals)

		end

	      with Not_found ->
		(* This is not a reference to a known global identifier. *)
		  EApp (EVar id, self#exprs actuals)
	    end
	| _ ->
	    (* The thing in function position is not a variable. *)
	    super#eapp e actuals

      method efun ps e =
	(* Check that local identifiers do not shadow global ones. *)
	assert (List.for_all self#patok ps);
	super#efun ps e

      method binding ((p, _) as binding) =
	(* Check that local identifiers do not shadow global ones. *)
	assert (self#patok p);
	super#binding binding

      method branch b =
	(* Check that local identifiers do not shadow global ones. *)
	assert (self#patok b.branchpat);
	super#branch b

      method patok = function
	| PWildcard
	| PUnit
	| PData _ ->
	    true
	| PVar id ->
	    let ok = not (Hashtbl.mem table id) in
	    if not ok then
	      Printf.fprintf stderr "SHADOWED: %s\n%!" id; (* TEMPORARY *)
	    ok
	| PTuple pats
	| POr pats ->
	    List.for_all self#patok pats
	| PAnnot (p, _) ->
            self#patok p

    end
  in

  (* Initialize the queue with all public definitions, and work from
     there. *)

  List.iter (function { valpublic = public } as def ->
    if public then
      enqueue def
  ) defs;

  let valdefs =
    Misc.qfold (fun defs def ->
      o#valdef def :: defs
    ) [] queue
  in

  Error.logC 1 (fun f ->
    Printf.fprintf f "%d functions before inlining, %d functions after inlining.\n"
       before (List.length valdefs));
  
  Time.tick "Inlining";

  { p with valdefs = valdefs }

(* The external entry point. *)

let inline p =
  if Settings.code_inlining then inline p else p

