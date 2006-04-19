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

(* Driver for the back-end. *)

open UnparameterizedSyntax

let program =
  Code.program

(* Perform inlining. *)

let program =
  if Settings.code_inlining then
    let program = Inliner.inline program in
    Time.tick "Inlining";
    program
  else
    program

(* Emit the code. *)

let () =
  let module P = Printer.Make (struct
    let filename = Settings.base ^ ".ml"
    let f = open_out filename
    let locate_stretches =
      if Settings.infer then
	(* Typechecking should not fail at this stage. Omit #line directives. *)
	None
      else
	Some (Filename.basename filename)
    let raw_stretch_action = false
  end) in
  P.program program

let () =
  Interface.write()

let () =
  Time.tick "Printing"

