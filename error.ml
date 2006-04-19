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

open Printf
open Lexing

(* TEMPORARY Vérifier que les messages d'erreur sont standardisés au
   maximum, localisés au maximum. Supprimer autant de fonctions que
   possible dans ce module. *)

(* TEMPORARY reprendre compl`etement implementation et interface
   de ce module *)

let log kind verbosity msg =
  if kind >= verbosity then
    Printf.fprintf stderr "%t%!" msg

let logG =
  log Settings.logG

let logA =
  log Settings.logA

let logC =
  log Settings.logC

let get_initialized_ref ref =
  match !ref with
  | None ->
      assert false
  | Some contents ->
      contents

let basename =
  ref (None : string option)

let filemark =
  ref Mark.none

let set_filename name =
  basename := Some (Filename.basename name);
  filemark := Mark.fresh()

let get_basename () =
  get_initialized_ref basename

let get_filemark () =
  !filemark

let errors =
  ref false

let printN positions message = 
  List.iter (fun position -> 
    fprintf stderr "%s:\n" (Positions.string_of_pos position)
  ) positions;
  fprintf stderr "%s\n%!" message

let errorN positions message =
  printN positions (Printf.sprintf "Error: %s" message);
  exit 1

let errorp v message =
  errorN [ Positions.position v ] message

let error1 position message =
  errorN [ Positions.lex_join position position ] message

let error message =
  errorN [] message

let signalN positions message =
  printN positions message;
  errors := true

let signal position1 position2 message =
  signalN [ Positions.lex_join position1 position2 ] message

let signalp v message =
  signalN [ Positions.position v ] message

let file =
  ref (None : in_channel option)

let get_file () =
  get_initialized_ref file

let warningN positions message =
  printN positions (Printf.sprintf "Warning: %s" message)

let warningp v message =
  warningN [ Positions.position v ] message

let warning2 position1 position2 message =
  warningN [ Positions.lex_join position1 position2 ] message

let warning message =
  warningN [] message

let errors () =
  !errors

