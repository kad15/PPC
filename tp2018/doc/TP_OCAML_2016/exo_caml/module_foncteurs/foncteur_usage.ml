(*

Comment utiliser un foncteur ?

La librairie standard définit le module Set, qui fournit un foncteur Make. Ce foncteur requiert un argument, qui est un module contenant (au moins) deux choses: le type des éléments donné par t et la fonction de comparaison donnée par compare . L'important est de s'assurer que la même fonction de comparaison sera toujours utilisée, même si le programmeur commet une erreur.

*)

(*si on veut un ensemble d'entiers'*)
(* on passe au functer un module.
on a vu qu'un module est commence par struct et se termine par end'
 *)
module Int_set = Set.Make (struct
                             type t = int
                             let compare = compare
                           end) 


on peut aussi d'abord ecrire :

module Toto = struct 
                type t = int
                let compare = compare
              end;;

puis

module Mon_set = Set.Make (Toto)


