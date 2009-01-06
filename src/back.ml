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

(* Driver for the back-end. *)

open UnparameterizedSyntax

(* Define an .ml file writer . *)

let write program =
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

(* Construct the code, using either the table-based or the code-based
   back-end, and pass it on to the printer. (This continuation-passing
   style is imposed by the fact that there is no conditional in ocaml's
   module language.) *)

let () =
  if Settings.table then
    let module B = TableBackend.Run (struct end) in
    write B.program
  else
    let module B = CodeBackend.Run (struct end) in
    write (Inliner.inline B.program)

(* Write the interface file. *)

let () =
  Interface.write()

let () =
  Time.tick "Printing"

