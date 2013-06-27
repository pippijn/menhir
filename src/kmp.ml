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

(* An implementation of Knuth, Morris, and Pratt's algorithm for
   searching for a pattern in a string. *)

(* The pattern [p] is considered a finite array of length [m]. *)

(* The string [s] is considered a finite or infinite array. It is
   represented as an accessor function, which accepts an integer index
   and either returns a character or raises the exception
   [OutOfBounds]. *)

exception OutOfBounds

(* The characters in the pattern and the string are integers. *)

(* TEMPORARY This relation need not be
   equality. In particular, a certain character in the pattern may match
   multiple characters in the string, and a certain character in the
   string may match multiple characters in the pattern. In other words,
   both the pattern and the string may contain wildcards. *)

(* If the search succeeds, it returns the offset in the string where the
   pattern was found. *)

module MutableBitSetAsInt = struct

  type t =
      int

  let create n =
    if n < Sys.word_size then
      0
    else
      raise (Invalid_argument "MutableBitSet.create")

  let init n f =
    let rec loop i mask s =
      (* [mask] is two to the [i] *)
      if i = n then
	s
      else
	loop (i+1) (mask lsl 1)	(if f i then mask lor s else s)
    in
    loop 0 1 (create n)

  let conjunction s1 s2 =
    s1 land s2

  let shift s =
    (s lsl 1) lor 1 (* excess bits on the left are ok *)

  let get i s =
    s land (1 lsl i) <> 0

end

module MutableBitSetAsIntArray = struct

  type t =
      int array

  let w =
    Sys.word_size - 1

  let msb_mask =
    1 lsl (w - 1)

  let create n =
    Array.create (if n mod w = 0 then n / w else n / w + 1) 0

  let init n f =
    let s = create n in

    (* In the following loop, [i] is the logical index that we are
       considering; [q] is [i / w]; [r] is [i mod w], or possibly [w];
       [mask] is two to the [r]; and [current] is the integer bit
       field that we are preparing in order to eventually write to
       [s.(q)]. *)

    let rec loop i q r mask current =
      if i = n then begin
	s.(q) <- current;
	s
      end
      else if r = w then begin
	s.(q) <- current;
	let q = q + 1 in
	loop i q 0 1 0
      end
      else
	loop (i+1) q (r+1) (mask lsl 1) (if f i then mask lor current else current)

    in
    loop 0 0 0 1 0

  let conjunction s1 s2 =
    let width = Array.length s1 in
    for q = 0 to width-1 do
      s1.(q) <- s1.(q) land s2.(q)
    done;
    s1

  let shift s =
    let width = Array.length s in
    let carry = ref 1 in
    for q = 0 to width-1 do
      let current = s.(q) in
      let next_carry = if current land msb_mask = 0 then 0 else 1 in
      let current = current lsl 1 in (* excess bits on the left are ok *)
      let current = current lor !carry in
      s.(q) <- current;
      carry := next_carry
    done;
    s

  let get i s =
    let q = i / w
    and r = i mod w in
    s.(q) land (1 lsl r) <> 0

end

let rec nodup1 x ys =
  match ys with
  | [] ->
      []
  | y :: ys ->
      if x = y then
	nodup1 x ys
      else
	y :: nodup1 y ys

let nodup xs =
  match xs with
  | [] ->
      []
  | x :: xs ->
      x :: nodup1 x xs

let rec search compare key data =
  let rec loop i j =
    if i = j then
      raise Not_found
    else
      let m = i + (j - i) / 2 in
      let key', datum = data.(m) in
      let c = compare key key' in
      if c = 0 then
	datum
      else if c < 0 then
	loop i m
      else
	loop (m+1) j
  in
  loop 0 (Array.length data)

module Make (MutableBitSet : sig
  type t
  val create: int -> t (* initialized at 0 everywhere *)
  val init: int -> (int -> bool) -> t (* initialized as the user desires *)
  val conjunction: t -> t -> t (* in-place conjunction on left argument *)
  val shift: t -> t (* in-place left shift by 1; also sets the lsb *)
  val get: int -> t -> bool (* read a bit *)
end)
= struct

  type state =
      MutableBitSet.t

  open MutableBitSet

  let kmp
    (p : int array)
    (s : int -> int)
    : int option =

    let m = Array.length p in

    (* The transition mask associated with a nonzero symbol [a]. *)

    let transition_mask a =
      MutableBitSet.init m (fun i ->
	p.(i) = 0 || p.(i) = a
      )
    in
    
    (* The transition mask associated with a symbol [a] that is not
       among the [p.(i)]'s. *)

    let default_transition_mask =
      MutableBitSet.init m (fun i ->
	p.(i) = 0
      )
    in

    (* Precompute a map of the nonzero characters [a] that
       appear in the pattern to their transition mask. *)

    let nonzero_chars =
      Array.fold_left (fun accu a ->
	if a = 0 then accu else a :: accu
      ) [] p
    in
    let nonzero_chars =
      Array.of_list (nodup (List.fast_sort Pervasives.compare nonzero_chars))
      (* TEMPORARY could allocate less memory by using a version of heapsort
	 that (filters out zero elements and?) removes duplicates on the fly? *)
    in
    let transition_masks =
      Array.map (fun a ->
	(a, transition_mask a)
      ) nonzero_chars
    in
    let transition_mask a : MutableBitSet.t =
      try
	search Pervasives.compare a transition_masks
      with Not_found ->
	default_transition_mask
    in

    (* Define the automaton's transition function. *)

    (* The integer index [i] represents the current state of the
       automaton. It is comprised between [0] (inclusive) and [m]
       (exclusive). The symbol [a] is the character that we are
       consuming. *)

    let transition (is : state) (a : int) : state =
      (* Apply the transition mask associated with the symbol [a].
	 This kills all states [i] that do not have an outgoing
	 transition along [a]. If [a] is the wildcard [0], nothing is killed. *)
      let is =
	if a = 0 then
	  is
	else
	  conjunction is (transition_mask a)
      in
      (* Shift the states that remain. This reflects the transitions
	 that are succesfully taken, as well as the loop from state 0
	 to itself. *)
      shift is
    in

    (* Define the automaton's initial state. *)

    let is =
      MutableBitSet.init m (fun i -> i = 0)
    in

    (* Search. *)

    (* The variable [i] is the automaton's current state. *)

    (* The variable [j] is the offset in the string [s] of the symbol
       that the automaton is about to consume. *)

    let rec loop is j =
      if get m is then     (* [m] is the accepting state *)
	Some (j - m)       (* success; return match offset *)
      else
	loop (transition is (s j)) (j+1)
    in

    try
      loop is 0
    with OutOfBounds ->
      None

end

module K = Make(MutableBitSetAsIntArray)

let wrap s =
  let n = Array.length s in
  fun i ->
    if i < n then
      s.(i)
    else
      raise OutOfBounds

let wrap_infinite s =
  let n = Array.length s in
  fun i ->
    if i < n then
      s.(i)
    else
      0

let test =
  K.kmp [| 0; 1; 2 |] (wrap [| 3; 1; 0; 1; 2; 3; 0 |])

