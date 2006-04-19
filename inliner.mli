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

(* This transformer inlines every function that is called at most
   once. It also inlines some functions whose body consists of a
   single function call. At the same time, every function that is
   never called is dropped. Public functions are never inlined or
   dropped. *)

val inline: IL.program -> IL.program


