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

(* Code for traversing or transforming [IL] terms. *)

open IL
open CodeBits

(* This turns a list of value definitions into a hash table. It also
   counts and numbers the definitions. We assume that the left-hand
   side of every definition is a variable. *)

let tabulate_defs (defs : valdef list) : int * (string, int * valdef) Hashtbl.t =
  let count = ref 0 in
  let table = Hashtbl.create 1023 in
  List.iter (fun def ->
    let k = !count in
    count := k + 1;
    Hashtbl.add table (pat2var def.valpat) (k, def)
  ) defs;
  !count, table

(* A class that helps transform expressions. *)

exception NoChange

class map = object (self)

  method expr e =
    try
      match e with
      | EVar x ->
	  self#evar x
      | EFun (ps, e) ->
	  self#efun ps e
      | EApp (e, es) ->
	  self#eapp e es
      | ELet (bs, e) ->
	  self#elet bs e
      | EMatch (e, bs) ->
	  self#ematch e bs
      | EIfThen (e, e1) ->
	  self#eifthen e e1
      | EIfThenElse (e, e1, e2) ->
	  self#eifthenelse e e1 e2
      | ERaise e ->
	  self#eraise e
      | ETry (e, bs) ->
	  self#etry e bs
      | EUnit ->
	  self#eunit
      | EIntConst k ->
	  self#eintconst k
      | EStringConst s ->
	  self#estringconst s
      | EData (d, es) ->
	  self#edata d es
      | ETuple es ->
	  self#etuple es
      | EAnnot (e, t) ->
	  self#eannot e t
      | EMagic e ->
	  self#emagic e
      | ERecord fs ->
	  self#erecord fs
      | ERecordAccess (e, f) ->
	  self#erecordaccess e f
      | ERecordWrite (e, f, e1) ->
	  self#erecordwrite e f e1
      | ETextual action ->
	  self#etextual action
      | EComment (s, e) ->
	  self#ecomment s e
      | EPatComment (s, p, e) ->
	  self#epatcomment s p e
    with NoChange ->
      e

  method evar x =
    raise NoChange

  method efun ps e =
    let e' = self#expr e in
    if e == e' then
      raise NoChange
    else
      EFun (ps, e')

  method eapp e es =
    let e' = self#expr e
    and es' = self#exprs es in
    if e == e' && es == es' then
      raise NoChange
    else
      EApp (e', es')

  method elet bs e =
    let bs' = self#bindings bs
    and e' = self#expr e in
    if bs == bs' && e == e' then
      raise NoChange
    else
      ELet (bs', e')

  method ematch e bs =
    let e' = self#expr e
    and bs' = self#branches bs in
    if e == e' && bs == bs' then
      raise NoChange
    else
      EMatch (e', bs')

  method eifthen e e1 =
    let e' = self#expr e
    and e1' = self#expr e1 in
    if e == e' && e1 == e1' then
      raise NoChange
    else
      EIfThen (e', e1')

  method eifthenelse e e1 e2 =
    let e' = self#expr e
    and e1' = self#expr e1
    and e2' = self#expr e2 in
    if e == e' && e1 == e1' && e2 == e2' then
      raise NoChange
    else 
      EIfThenElse (e', e1', e2')

  method eraise e =
    let e' = self#expr e in
    if e == e' then
      raise NoChange
    else
      ERaise e'

  method etry e bs =
    let e' = self#expr e
    and bs' = self#branches bs in
    if e == e' && bs == bs' then
      raise NoChange
    else
      ETry (e', bs')

  method eunit =
    raise NoChange

  method eintconst k =
    raise NoChange

  method estringconst s =
    raise NoChange

  method edata d es =
    let es' = self#exprs es in
    if es == es' then
      raise NoChange
    else
      EData (d, es')

  method etuple es =
    let es' = self#exprs es in
    if es == es' then
      raise NoChange
    else
      ETuple es'

  method eannot e t =
    let e' = self#expr e in
    if e == e' then
      raise NoChange
    else
      EAnnot (e', t)

  method emagic e =
    let e' = self#expr e in
    if e == e' then
      raise NoChange
    else
      EMagic e'

  method erecord fs =
    let fs' = self#fields fs in
    if fs == fs' then
      raise NoChange
    else
      ERecord fs'

  method erecordaccess e f =
    let e' = self#expr e in
    if e == e' then
      raise NoChange
    else
      ERecordAccess (e', f)

  method erecordwrite e f e1 =
    let e' = self#expr e
    and e1' = self#expr e1 in
    if e == e' && e1 == e1' then
      raise NoChange
    else
      ERecordWrite (e', f, e1')

  method etextual action =
    raise NoChange
 
  method ecomment s e =
    let e' = self#expr e in
    if e == e' then
      raise NoChange
    else
      EComment (s, e')

  method epatcomment s p e =
    let e' = self#expr e in
    if e == e' then
      raise NoChange
    else
      EPatComment (s, p, e')

  method exprs es =
    Misc.smap self#expr es

  method fields fs =
    Misc.smap self#field fs

  method field ((f, e) as field) =
    let e' = self#expr e in
    if e == e' then
      field
    else
      (f, e')

  method branches bs =
    Misc.smap self#branch bs

  method branch b =
    let e = b.branchbody in
    let e' = self#expr e in
    if e == e' then
      b
    else
      { b with branchbody = e' }

  method bindings bs =
    Misc.smap self#binding bs

  method binding ((p, e) as b) =
    let e' = self#expr e in
    if e == e' then
      b
    else
      (p, e')

  method valdef def =
    let e = def.valval in
    let e' = self#expr e in
    if e == e' then
      def
    else
      { def with valval = e' }

  method valdefs defs =
    Misc.smap self#valdef defs

end

(* A class that helps iterate, or fold, over expressions. *)

class ['a] fold = object (self)

  method expr (accu : 'a) e =
    match e with
    | EVar x ->
	self#evar accu x
    | EFun (ps, e) ->
	self#efun accu ps e
    | EApp (e, es) ->
	self#eapp accu e es
    | ELet (bs, e) ->
	self#elet accu bs e
    | EMatch (e, bs) ->
	self#ematch accu e bs
    | EIfThen (e, e1) ->
	self#eifthen accu e e1
    | EIfThenElse (e, e1, e2) ->
	self#eifthenelse accu e e1 e2
    | ERaise e ->
	self#eraise accu e
    | ETry (e, bs) ->
	self#etry accu e bs
    | EUnit ->
	self#eunit accu
    | EIntConst k ->
	self#eintconst accu k
    | EStringConst s ->
	self#estringconst accu s
    | EData (d, es) ->
	self#edata accu d es
    | ETuple es ->
	self#etuple accu es
    | EAnnot (e, t) ->
	self#eannot accu e t
    | EMagic e ->
	self#emagic accu e
    | ERecord fs ->
	self#erecord accu fs
    | ERecordAccess (e, f) ->
	self#erecordaccess accu e f
    | ERecordWrite (e, f, e1) ->
	self#erecordwrite accu e f e1
    | ETextual action ->
	self#etextual accu action
    | EComment (s, e) ->
	self#ecomment accu s e
    | EPatComment (s, p, e) ->
	self#epatcomment accu s p e

  method evar (accu : 'a) x =
    accu

  method efun (accu : 'a) ps e =
    let accu = self#expr accu e in
    accu

  method eapp (accu : 'a) e es =
    let accu = self#expr accu e in
    let accu = self#exprs accu es in
    accu

  method elet (accu : 'a) bs e =
    let accu = self#bindings accu bs in
    let accu = self#expr accu e in
    accu

  method ematch (accu : 'a) e bs =
    let accu = self#expr accu e in
    let accu = self#branches accu bs in
    accu

  method eifthen (accu : 'a) e e1 =
    let accu = self#expr accu e in
    let accu = self#expr accu e1 in
    accu

  method eifthenelse (accu : 'a) e e1 e2 =
    let accu = self#expr accu e in
    let accu = self#expr accu e1 in
    let accu = self#expr accu e2 in
    accu

  method eraise (accu : 'a) e =
    let accu = self#expr accu e in
    accu

  method etry (accu : 'a) e bs =
    let accu = self#expr accu e in
    let accu = self#branches accu bs in
    accu

  method eunit (accu : 'a) =
    accu

  method eintconst (accu : 'a) k =
    accu

  method estringconst (accu : 'a) s =
    accu

  method edata (accu : 'a) d es =
    let accu = self#exprs accu es in
    accu

  method etuple (accu : 'a) es =
    let accu = self#exprs accu es in
    accu

  method eannot (accu : 'a) e t =
    let accu = self#expr accu e in
    accu

  method emagic (accu : 'a) e =
    let accu = self#expr accu e in
    accu

  method erecord (accu : 'a) fs =
    let accu = self#fields accu fs in
    accu

  method erecordaccess (accu : 'a) e f =
    let accu = self#expr accu e in
    accu

  method erecordwrite (accu : 'a) e f e1 =
    let accu = self#expr accu e in
    let accu = self#expr accu e1 in
    accu

  method etextual (accu : 'a) action =
    accu

  method ecomment (accu : 'a) s e =
    let accu = self#expr accu e in
    accu

  method epatcomment (accu : 'a) s p e =
    let accu = self#expr accu e in
    accu

  method exprs (accu : 'a) es =
    List.fold_left self#expr accu es

  method fields (accu : 'a) fs =
    List.fold_left self#field accu fs

  method field (accu : 'a) (f, e) =
    let accu = self#expr accu e in
    accu

  method branches (accu : 'a) bs =
    List.fold_left self#branch accu bs

  method branch (accu : 'a) b =
    let accu = self#expr accu b.branchbody in
    accu

  method bindings (accu : 'a) bs =
    List.fold_left self#binding accu bs

  method binding (accu : 'a) (p, e) =
    let accu = self#expr accu e in
    accu

  method valdef (accu : 'a) def =
    let accu = self#expr accu def.valval in
    accu

  method valdefs (accu : 'a) defs =
    List.fold_left self#valdef accu defs

end

