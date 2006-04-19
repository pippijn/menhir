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

(* This module defines the interface of the generated parser. *)

(* This is the [Error] exception. *)

val excname: string
val excdef: IL.excdef

(* The type of the entry point for the nonterminal start symbol
   [symbol]. *)

val entrytypescheme: string -> IL.typescheme

(* This writes the interface of the generated parser to the [.mli]
   file. *)

val write: unit -> unit

