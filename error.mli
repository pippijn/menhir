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

(* This module helps report errors and maintains some information
   about the source file that is being read. *)

(* Call [set_filename] before lexing and parsing in order to inform
   the module [Error] about the name of the file that is being
   examined. *)

(* TEMPORARY limiter ou supprimer cette interface stateful; + cleanup general necessaire *)

val set_filename: string -> unit

val get_basename: unit -> string

val get_filemark: unit -> Mark.t

val logG: int -> (out_channel -> unit) -> unit
val logA: int -> (out_channel -> unit) -> unit
val logC: int -> (out_channel -> unit) -> unit

(* [error1 p msg] displays the error message [msg], referring to
   position [p], and exits. *)

val error1: Lexing.position -> string -> 'a

val errorN: Positions.t list -> string -> 'a

val file: in_channel option ref

val get_file: unit -> in_channel

(* [warning2 p1 p2 msg] displays the warning message [msg], referring
   to the position range [p1--p2]. *)

val warning2: Lexing.position -> Lexing.position -> string -> unit

(* [errorp v msg] displays the error message [msg], referring to the
   position range carried by [v], and exits. *)

val errorp: 'a Positions.located -> string -> 'b

(* [warningp v msg] displays the warning message [msg], referring to
   the position range carried by [v]. *)

val warningp: 'a Positions.located -> string -> unit

val warning: string -> unit

val warningN: Positions.t list -> string -> unit

val error: string -> 'a

(* [errors] returns [true] if [signal], [signalp] or [signalN] was previously
   called. *)

val signalN: Positions.t list -> string -> unit
val signal: Lexing.position -> Lexing.position -> string -> unit
val signalp: 'a Positions.located -> string -> unit
val errors: unit -> bool

