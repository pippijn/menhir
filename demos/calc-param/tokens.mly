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

(* We want the tokens to be independent of the [Semantics] parameter,
   so we declare them here, in a separate file, as opposed to within
   [parser.mly]. *)

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%%

