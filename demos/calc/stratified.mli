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

exception Error

type token = 
  | TIMES
  | RPAREN
  | PLUS
  | MINUS
  | LPAREN
  | INT of (int)
  | EOL
  | DIV


val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)