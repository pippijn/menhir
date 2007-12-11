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

(* This module performs some code sharing. It is typically able to share
   the [run] functions for states that have identical transition tables.
   Its implementation is ad hoc. It was deemed easier to implement a
   separate pass than to modify the code generator to perform sharing
   in the first place. *)

val share: IL.program -> IL.program

