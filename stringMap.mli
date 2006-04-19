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

include Map.S with type key = string

val cardinal : 'a t -> int 

(* [restrict s m] restricts the domain of the map [m] to (its
   intersection with) the set [s]. *)

val restrict: StringSet.t -> 'a t -> 'a t 

(* [filter pred m] restricts the domain of the map [m] to 
   (key, value) couples that verify [pred]. *)

val filter: (string -> 'a -> bool) -> 'a t -> 'a t

(* [domain m] returns the domain of the map [m]. *)
val domain: 'a t -> StringSet.t
