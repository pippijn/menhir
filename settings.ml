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

(* ------------------------------------------------------------------------- *)
(* Prepare for parsing the command line. *)

type token_type_mode =
  | TokenTypeAndCode   (* produce the definition of the [token] type and code for the parser *)
  | TokenTypeOnly      (* produce the type definition only *)
  | CodeOnly of string (* produce the code only; import token type from specified module *)

let token_type_mode =
  ref TokenTypeAndCode

let tokentypeonly () =
  token_type_mode := TokenTypeOnly

let codeonly m =
  if String.capitalize m <> m then begin
    (* Not using module [Error] to avoid a circular dependency. *)
    Printf.fprintf stderr "Error: %s is not a valid Objective Caml module name.\n" m;
    exit 1
  end;
  token_type_mode := CodeOnly m

let version =
  ref false

let pager =
  ref true

let explain =
  ref false

let base =
  ref ""

let dump =
  ref false

let graph =
  ref false

let trace =
  ref false

let noprefix =
  ref false

let preprocess_only =
  ref false

let recovery =
  ref false

let v () =
  dump := true;
  explain := true

let infer =
  ref false

let inline =
  ref true

type ocamldep_mode =
  | OMNone        (* do not invoke ocamldep *)
  | OMRaw         (* invoke ocamldep and echo its raw output *)
  | OMPostprocess (* invoke ocamldep and postprocess its output *)

let depend =
  ref OMNone

let code_inlining =
  ref true

let comment =
  ref false

let ocamlc =
  ref "ocamlc"

let ocamldep =
  ref "ocamldep"

let logG, logA, logC =
  ref 0, ref 0, ref 0

let timings =
  ref false

let filenames = 
  ref StringSet.empty

let no_stdlib = 
  ref false

let stdlib_path =
  ref Stdlib.path

let insert name = 
  filenames := StringSet.add name !filenames

let options = Arg.align [
  "--base", Arg.Set_string base, "<basename> Specifies a base name for the output file(s)";
  "--comment", Arg.Set comment, " Include comments in the generated code";
  "--depend", Arg.Unit (fun () -> depend := OMPostprocess), " Invoke ocamldep and display dependencies";
  "--dump", Arg.Set dump, " Describe the automaton in <basename>.automaton";
  "--error-recovery", Arg.Set recovery, " Attempt recovery by discarding tokens after errors";
  "--explain", Arg.Set explain, " Explain conflicts in <basename>.conflicts";
  "--external-tokens", Arg.String codeonly, "<module> Import token type definition from <module>";
  "--graph", Arg.Set graph, " Write the grammar's dependency graph to <basename>.dot";
  "--infer", Arg.Set infer, " Invoke ocamlc for ahead of time type inference";
  "--log-automaton", Arg.Set_int logA, "<level> Log information about the automaton";
  "--log-code", Arg.Set_int logC, "<level> Log information about the generated code";
  "--log-grammar", Arg.Set_int logG, "<level> Log information about the grammar";
  "--no-code-inlining", Arg.Clear code_inlining, " (undocumented)";
  "--no-inline", Arg.Clear inline, " Ignore the %inline keyword.";
  "--no-pager", Arg.Clear pager, " (undocumented)";
  "--no-prefix", Arg.Set noprefix, " (undocumented)";
  "--no-stdlib", Arg.Set no_stdlib, " Do not load the standard library";
  "--ocamlc", Arg.Set_string ocamlc, "<command> Specifies how ocamlc should be invoked";
  "--ocamldep", Arg.Set_string ocamldep, "<command> Specifies how ocamldep should be invoked";
  "--only-preprocess", Arg.Set preprocess_only, " Print a simplified grammar and exit";
  "--only-tokens", Arg.Unit tokentypeonly, " Generate token type definition only, no code";
  "--raw-depend", Arg.Unit (fun () -> depend := OMRaw), " Invoke ocamldep and echo its raw output";
  "--timings", Arg.Set timings, " Display internal timings";
  "--trace", Arg.Set trace, " Include tracing instructions in the generated code";
  "--stdlib", Arg.Set_string stdlib_path, "<directory> Specify where the standard library lies";
  "--version", Arg.Set version, " Show version number and exit";
  "-b", Arg.Set_string base, "<basename> Synonymous with --base <basename>";
  "-lg", Arg.Set_int logG, " Synonymous with --log-grammar";
  "-la", Arg.Set_int logA, " Synonymous with --log-automaton";
  "-lc", Arg.Set_int logC, " Synonymous with --log-code";
  "-v", Arg.Unit v, " Synonymous with --dump --explain";
]

let usage =
  Printf.sprintf "Usage: %s <options> <filenames>" Sys.argv.(0)

(* ------------------------------------------------------------------------- *)
(* Parse the command line. *)

let () =
  Arg.parse options insert usage

let () =
  if !version then begin
    Printf.printf "menhir, version %s\n" Version.version;
    exit 0
  end

(* ------------------------------------------------------------------------- *)
(* Export the settings. *)

let stdlib_filename = 
  !stdlib_path ^ "/standard.mly"

let filenames =
  StringSet.elements !filenames

let base =
  if !base = "" then
    match filenames with
    | [] ->
	Printf.fprintf stderr "%s\n" usage;
	exit 1
    | [ filename ] ->
	Filename.chop_suffix filename ".mly"
    | _ ->
	Printf.fprintf stderr "Error: you must specify --base when providing multiple input files.\n";
	exit 1
  else
    !base

let filenames = 
  if !no_stdlib then
    filenames
  else 
    stdlib_filename :: filenames

let token_type_mode =
  !token_type_mode

let pager =
  !pager

let explain =
  !explain

let dump =
  !dump

let graph =
  !graph

let trace =
  !trace

let recovery =
  !recovery

let noprefix =
  !noprefix

let infer =
  !infer

let code_inlining =
  !code_inlining

let depend =
  !depend

let inline =
  !inline

let comment =
  !comment

let preprocess_only =
  !preprocess_only

let ocamlc =
  !ocamlc

let ocamldep =
  !ocamldep

let logG, logA, logC =
  !logG, !logA, !logC

let timings =
  !timings

