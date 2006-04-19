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

let times =
  ref (Unix.times())

let tick msg =
  if Settings.timings then
    let times1 = !times in
    let times2 = Unix.times() in
    Printf.fprintf stderr "%s: %.02fs\n"
      msg
      (times2.Unix.tms_utime -. times1.Unix.tms_utime);
    times := Unix.times()

