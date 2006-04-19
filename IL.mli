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

(* Abstract syntax of the language used for code production. *)

type program = {

    (* Functor parameters. *)
    paramdefs: Stretch.t list;

    (* Raw Objective Caml prologue. *)
    prologue: Stretch.t list;

    (* Exception definitions. *)
    excdefs: excdef list;
    
    (* Algebraic data type definitions (mutually recursive). *)
    typedefs: typedef list;

    (* Value definitions (not mutually recursive). *)
    nonrecvaldefs: valdef list;

    (* Function definitions (mutually recursive). *)
    valdefs: valdef list;

    (* Raw Objective Caml postlogue. *)
    postlogue: string list;

  } 

and interface = {

    (* Functor parameters. *)
    paramdecls: Stretch.t list;

    (* Exception definitions. *)
    excdecls: excdef list;
    
    (* Algebraic data type declarations (mutually recursive). *)
    typedecls: typedef list;

    (* Value declarations. *)
    valdecls: (string * typescheme) list

  } 

and excdef = {

    (* Name of the exception. *)
    excname: string;

  }

and typedef = {

    (* Name of the algebraic data type. *)
    typename: string;

    (* Type parameters. This is a list of type variable names,
       without the leading quote, which will be added by the
       pretty-printer. *)
    typeparams: string list;

    (* Data constructors. *)
    typerhs: typedefrhs;

    (* Constraint. *)
    typeconstraint: (typ * typ) option

  } 

and typedefrhs =
  | TDefRecord of fielddef list
  | TDefSum of datadef list

and fielddef = {

    (* Whether the field is mutable. *)
    modifiable: bool;

    (* Name of the field. *)
    fieldname: string;

    (* Type of the field. *)
    fieldtype: typescheme

  }  

and datadef = {

    (* Name of the data constructor. *)
    dataname: string;

    (* Types of the value parameters. *)
    datavalparams: typ list;

    (* Instantiated type parameters, if this is a GADT --
       [None] if this is an ordinary ADT. *)
    datatypeparams: typ list option;

  } 

and typ =
  
  (* Textual Objective Caml type. *)
  | TypTextual of Stretch.ocamltype

  (* Type variable, without its leading quote. *)
  | TypVar of string

  (* Application of an algebraic data type constructor. *)
  | TypApp of string * typ list

  (* Anonymous tuple. *)
  | TypTuple of typ list

  (* Arrow type. *)
  | TypArrow of typ * typ

and typescheme = {

  (* Universal quantifiers, without leading quotes. *)
  quantifiers: string list;

  (* Body. *)
  body: typ;

  } 

and valdef = {

  (* Whether the value is public. Public values cannot be
     suppressed by the inliner. They serve as seeds for the
     dead code analysis. *)

  valpublic: bool;

  (* Definition's left-hand side. *)
  valpat: pattern;

  (* Value to which it is bound. *)
  valval: expr

  } 

and expr =

  (* Variable. *)
  | EVar of string

  (* Function. *)
  | EFun of pattern list * expr

  (* Function call. *)
  | EApp of expr * expr list

  (* Local definitions. This is a nested sequence of [let]
     definitions. *)
  | ELet of (pattern * expr) list * expr

  (* Case analysis. *)
  | EMatch of expr * branch list
  | EIfThen of expr * expr
  | EIfThenElse of expr * expr * expr

  (* Raising exceptions. *)
  | ERaise of expr

  (* Exception analysis. *)
  | ETry of expr * branch list

  (* Data construction. Tuples of length 1 are considered nonexistent,
     that is, [ETuple [e]] is considered the same expression as [e]. *)

  | EUnit
  | EIntConst of int
  | EStringConst of string
  | EData of string * expr list
  | ETuple of expr list

  (* Type annotation. *)
  | EAnnot of expr * typescheme

  (* Cheating on the typechecker. *) (* TEMPORARY *)
  | EMagic of expr

  (* Records. *)
  | ERecord of (string * expr) list
  | ERecordAccess of expr * string
  | ERecordWrite of expr * string * expr

  (* Textual Objective Caml code. *)
  | ETextual of Stretch.t

  (* Comments. *)
  | EComment of string * expr
  | EPatComment of string * pattern * expr

and branch = {

  (* Branch pattern. *)
  branchpat: pattern;

  (* Branch body. *)
  branchbody: expr;

  } 

and pattern =

  (* Wildcard. *)
  | PWildcard

  (* Variable. *)
  | PVar of string

  (* Data deconstruction. Tuples of length 1 are considered nonexistent,
     that is, [PTuple [p]] is considered the same pattern as [p]. *)
  | PUnit
  | PData of string * pattern list
  | PTuple of pattern list

  (* Disjunction. *)
  | POr of pattern list

  (* Type annotation. *)
  | PAnnot of pattern * typ

