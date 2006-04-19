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

(* $Id: action.mli,v 1.6 2005/12/01 16:20:06 regisgia Exp $ *)

(** Semantic action's type. *)
type t

(** [compose x a1 a2] builds the action [let x = a1 in a2]. This
    feature is used during the processing of the %inline keyword. *)
val compose : string -> t -> t -> t

(** [rename phi a] builds the action [let x1 = x1' and ... xn = xn' in a] if
    [phi] is [ (x1, x1') ... (xn, xn') ]. *)
val rename: (string * string) list -> t -> t

(** Semantic actions are translated into [IL] code using the
    [IL.ETextual] and [IL.ELet] constructors. *)
val to_il_expr: t -> IL.expr

(** A semantic action might be the inlining of several others. The
    filenames of the different parts are given by [filenames a]. This
    can be used, for instance, to check whether all parts come from
    the standard library. *)
val filenames: t -> string list

(** [pkeywords a] returns a list of all keyword occurrences in [a]. *)
val pkeywords: t -> Keyword.keyword Positions.located list

(** [keywords a] is the set of keywords used in the semantic action [a]. *)
val keywords: t -> Keyword.KeywordSet.t

(** [print f a] prints [a] to channel [f]. *)
val print: out_channel -> t -> unit

(** [from_stretch s] builds an action out of a textual piece of code. *)
val from_stretch: Stretch.t -> t

(** Check whether the keyword $previouserror is used in the action. *)
val has_previouserror: t -> bool

(** Check whether the keyword $syntaxerror is used in the action. *)
val has_syntaxerror: t -> bool

(** Check whether the keyword $start is used in the action. *)
val has_leftstart: t -> bool

(** Check whether the keyword $end is used in the action. *)
val has_leftend: t -> bool

(** Check whether a particular $i keyword is used in the action. *)
val has_dollar: int -> t -> bool

(** Check whether any $i keyword is used in the action. *)
val use_dollar: t -> bool

