(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  Fran�ois Pottier and Yann R�gis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in file LICENSE.                                     *)
(*                                                                        *)
(**************************************************************************)

let map f = function
  | None ->
      None
  | Some x ->
      Some (f x)

let iter f o =
  match o with
  | None ->
      ()
  | Some x ->
      f x

let fold f o accu =
  match o with
  | None ->
      accu
  | Some x ->
      f x accu

