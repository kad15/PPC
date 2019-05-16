(* 

3 Utilisation des files et des piles
On va parcourir une structure d’arbre binaire en utilisant soit une file soit une pile.
1. Dans un nouveau fichier tree.ml, définir le type des arbres binaires constitués soit d’un
nœud associé à une donnée quelconque, soit de l’arbre vide.
2. On peut accéder à un module X à l’aide d’un alias avec la notation suivante :
module M = X
Renommer le module Fifo en Set.
3. Écrire la fonction iter sur les arbres binaires équivalente à la fonction List.iter sur les
listes en utilisant la structure de donnée fournie par le module Set :
— on initialise la structure en ajoutant la racine de l’arbre à la structure vide ;
— si la structure n’est pas vide, on retire un nœud, on traite la donnée associée à l’aide
de la fonction passée en paramètre à iter puis on appelle récursivement l’itérateur
sur la structure dans laquelle on aura ajouté les fils du nœud traité.
4. Appliquer cet itérateur à l’arbre suivant :

*)

open Fifo;;
type 'a t = 
  | Node of 'a * 'a t * 'a t
  | Empty

module Set = Fifo;;

let rec iter f t = match t with
    [] -> ()
  | a::l -> f a; iter f l


let iter f t = 
  let rec iter_rec nodes = 
    if not (Set.is_empty nodes) then
    (
      let (node,nodes) = Set.take nodes in
      match node with 
        Empty -> iter_rec nodes
      | Node (l,x,r) -> f x; iter_rec (Set.add l nodes))
    ) 
  in
  iter_rec (Set.add t Set.empty);;



let t = Node (Node (Node (Empty, 4, Empty), 2, Node (Empty, 5, Empty)),
              1,
              Node (Node (Empty, 6, Empty), 3, Empty))


let () = iter (fun x -> Printf.printf "%d " x) t; Printf.printf "\n";;

