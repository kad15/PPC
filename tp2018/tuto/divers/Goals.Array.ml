open Facile
open Easy

(*
Goals.Array.forall g [|e1; e2; ...; en|] = (g e1) &&~ (g e2) &&~ ... &&~ (g en)
Labeling of an array of variables is the iteration of the instantiation of 
one variable (Goals.indomain):
*)

let labeling_array = Goals.Array.forall Goals.indomain
(*
A matrix is an array of arrays; following the isomorphism, 
labeling of a matrix must be simply
a composition of the array iterator:
*)
let labeling_matrix = Goals.Array.forall labeling_array

