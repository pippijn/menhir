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

open Printf
open Syntax

let load_partial_grammar filename = 
  if Filename.check_suffix filename ".mly" then
    Error.set_filename filename
  else
    Error.error (sprintf "argument file names should end in .mly. \"%s\" is not accepted." filename);
  try

    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    Error.file := Some channel;
    lexbuf.Lexing.lex_curr_p <-
	{ 
	  Lexing.pos_fname = filename; 
	  Lexing.pos_lnum  = 1;
	  Lexing.pos_bol   = 0; 
	  Lexing.pos_cnum  = 0
	};
    let grammar =
      { (Parser.grammar Lexer.main lexbuf) with ConcreteSyntax.pg_filename = filename }
    in
    Error.file := None;
    close_in channel;

    (* If there were errors during parsing, stop. This has to be done
       explicitly here because the parser performs error recovery and
       does not die at the first error. One could even go further and
       attempt to work with the grammar in spite of the parse errors,
       but we choose not to. *)

    if Error.errors () then
      exit 1
    else
      grammar

  with Sys_error msg ->
    Error.error msg

let partial_grammars = 
  List.map load_partial_grammar Settings.filenames

let () =
  Time.tick "Lexing and parsing"

let parameterized_grammar = 
  PartialGrammar.join_partial_grammars partial_grammars

let grammar = 
  ParameterizedGrammar.expand parameterized_grammar

let () =
  Time.tick "Joining and expanding"

