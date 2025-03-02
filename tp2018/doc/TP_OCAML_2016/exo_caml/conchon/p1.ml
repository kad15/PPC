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

(* Programme 1 page 8 Année bissextile *)

let year = read_int ()
let leap =
	(year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0
let msg = if leap then "is" else "is not"
let () = Printf.printf "%d %s a leap year\n" year msg
