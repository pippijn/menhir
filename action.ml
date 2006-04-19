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

(* $Id: action.ml,v 1.6 2005/12/01 16:20:06 regisgia Exp $ *)

open Keyword

type t = 
    {
      expr	: IL.expr;
      keywords  : Keyword.KeywordSet.t;
      filenames : string list;
      pkeywords : Keyword.keyword Positions.located list
    }

let from_stretch s = 
  { 
    expr      = IL.ETextual s;
    filenames = [ s.Stretch.stretch_filename ];
    keywords  = Keyword.KeywordSet.from_list (List.map Positions.value s.Stretch.stretch_keywords);
    pkeywords = s.Stretch.stretch_keywords;
  }

let compose x a1 a2 = 
  {
    expr      = IL.ELet ([ IL.PVar x, a1.expr ], a2.expr);
    keywords  = Keyword.KeywordSet.union a1.keywords a2.keywords;
    filenames = a1.filenames @ a2.filenames;
    pkeywords = a1.pkeywords @ a2.pkeywords;
  }

let rename phi a = 
  { a with expr = IL.ELet (List.map (fun (x, x') -> (IL.PVar x, IL.EVar x')) phi, 
			   a.expr); 
  }

let to_il_expr action = 
  action.expr

let filenames action = 
  action.filenames

let keywords action = 
  action.keywords

let pkeywords action = 
  action.pkeywords


let rec print f action = 
  let module P = Printer.Make (struct let f = f 
				      let locate_stretches = None 
				      let raw_stretch_action = true
			       end) 
  in
    P.expr action.expr

let has_previouserror action =
  KeywordSet.mem PreviousError (keywords action)

let has_syntaxerror action =
  KeywordSet.mem SyntaxError (keywords action)

let has_leftstart action =
  KeywordSet.exists (function
    | Position (Left, WhereStart, _) ->
	true
    | _ ->
	false
  ) (keywords action)

let has_leftend action =
  KeywordSet.exists (function
    | Position (Left, WhereEnd, _) ->
	true
    | _ ->
	false
  ) (keywords action)

let has_dollar i action =
  KeywordSet.exists (function
    | Dollar j when i = j ->
	true
    | _ ->
	false
  ) (keywords action)

let use_dollar action = 
  KeywordSet.exists (function
    | Dollar _ ->
	true
    | _ ->
	false
  ) (keywords action)
    
    


