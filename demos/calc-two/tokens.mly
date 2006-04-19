/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            */
/*                                                                        */
/*  Copyright 2005 Institut National de Recherche en Informatique et      */
/*  en Automatique. All rights reserved. This file is distributed         */
/*  under the terms of the Q Public License version 1.0, with the         */
/*  change described in file LICENSE.                                     */
/*                                                                        */
/**************************************************************************/

(* This partial grammar specification defines the set of tokens. *)

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%%

