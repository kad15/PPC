(*
Réutiliser l’implémentation de pile fonctionnelle vue en cours (p. 155) en changeant le nom de la fonction pop en take.
Dans les fichiers d’implémentation lifo.ml et d’interface lifo.mli :
1. Ajouter la fonction add.
2. Ajouter la fonction is_empty.
3. Ajouter la fonction size.
Les modules Fifo et Lifo doivent posséder la même interface (hormis la fonction size).

*)

(* II - Pile fonctionnelle *)

type 'a t = {stack : 'a list; size : int}
 
exception Empty

let empty = {stack = []; size = 0}

let take s =
  match s.stack with
   [] -> raise Empty
  | h :: t -> (h, {stack = t; size = s.size - 1})



(* 1 *)

let add x s = {stack = x::s.stack; size = s.size + 1} ;;

(* 2 *)
let is_empty s = s.size =0 ;;


(* 3 *)
let size s = s.size;;


let iter f s = List.iter f s.stack
(* III - Utilisation des files et des piles *)

(* 1 *)


(* 2 *)


(* 3 *)

(* 4 *)  

(* 5 *) 
