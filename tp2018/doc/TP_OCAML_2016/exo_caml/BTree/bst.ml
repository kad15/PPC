(***********************************************************************)
(*                                                                     *)
(*  OCaml library from the book ``Apprendre à programmer avec OCaml''  *)
(*                                                                     *)
(*  Sylvain Conchon and Jean-Christophe Filliâtre                      *)
(*  Université Paris Sud                                               *)
(*                                                                     *)
(*  Copyright 2014 Université Paris Sud.  All rights reserved. This    *)
(*  file is distributed under the terms of the GNU Library General     *)
(*  Public License, with the same special exception on linking as the  *)
(*  OCaml library. See http://caml.inria.fr/ocaml/license.fr.html      *)
(*                                                                     *)
(***********************************************************************)
module type PersistentSet = sig
  type elt
  type t
  val empty : t
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val min_elt : t -> elt
  val remove : elt -> t -> t
  val cardinal : t -> int
end

module type Ordered = sig
  type t
  val compare: t -> t -> int
end

(* Programme 34 page 201
   Arbres binaires de recherche (1/2) *)

  type t = Empty | Node of t * elt * t

  let rec min_elt = function
    | Empty -> raise Not_found
    | Node (Empty, v, __) -> v
    | Node (l, _, _) -> min_elt l

  let rec mem x = function
    | Empty ->
	false
    | Node (l, v, r) ->
	let c = X.compare x v in
	c = 0 || if c < 0 then mem x l else mem x r

  let rec add x t =
    match t with
    | Empty ->
	Node (Empty, x, Empty)
    | Node (l, v, r) ->
        let c = X.compare x v in
	if c = 0 then t
	else if c < 0 then Node (add x l, v, r)
	else Node (l, v, add x r)


let rec remove_min_elt = function
    | Empty -> Empty
    | Node (Empty, _, r) -> r
    | Node (l, v, r) -> Node (remove_min_elt l, v, r)

  let merge t1 t2 = match t1, t2 with
    | Empty, t | t, Empty -> t
    | _ -> Node (t1, min_elt t2, remove_min_elt t2)

  let rec remove x = function
    | Empty ->
	Empty
    | Node (l, v, r) ->
	let c = X.compare x v in
	if c = 0 then merge l r
	else if c < 0 then Node (remove x l, v, r)
	else Node (l, v, remove x r)
