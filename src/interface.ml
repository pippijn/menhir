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

open UnparameterizedSyntax
open IL
open CodeBits
open TokenType

(* This is the [Error] exception. *)

let excname =
  "Error"

let excdef = {
  excname = excname;
  exceq = None;
}

let excredef = {
  excdef with exceq = Some excname
}


let exc2name =
  "StateError"

let exc2def = {
  excname = exc2name ^ " of " ^ tctoken ^ " * int";
  exceq = None;
}

let exc2redef = {
  excname = exc2name;
  exceq = Some exc2name;
}

(* The type of the entry point for the start symbol [symbol]. *)

let entrytypescheme symbol =
  let ocamltype =
    try
      StringMap.find symbol PreFront.grammar.types
    with Not_found ->
      (* Every start symbol should have a type. *)
      assert false
  in
  type2scheme (marrow [ arrow tlexbuf ttoken; tlexbuf ] (TypTextual ocamltype))

(* This is the interface of the generated parser. *)

let interface = {

  paramdecls =
    PreFront.grammar.parameters;

  excdecls =
    [ excdef; exc2def ];

  typedecls =
    tokentypedef;

  valdecls =
    StringSet.fold (fun symbol decls ->
      (Misc.normalize symbol, entrytypescheme symbol) :: decls
    ) PreFront.grammar.start_symbols []

} 

(* Writing the interface to a file. *)

let write () =
  let mli = open_out (Settings.base ^ ".mli") in
  let module P = Printer.Make (struct
    let f = mli
    let locate_stretches = None
    let raw_stretch_action = false
  end) in
  P.interface interface;
  close_out mli

