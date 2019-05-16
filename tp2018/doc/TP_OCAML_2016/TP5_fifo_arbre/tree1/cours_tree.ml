(* avec cette impémentation un arbre est noeud dont les fils sont des noeuds rangés dans une liste  ; feuilles neoud avec liste vide *)
type arbre = N of arbre list;;


(* on peut introduire un second constructeur si on veut différentier les feuilles *)
type arbre = F | N of arbre list;;

(* on peut ensuite ajouter une étiquette sur le noeuds par exemple de type polymorphe 'a *)
type 'a arbre = F of 'a | N of 'a * 'a arbre list;; 

(* si on veur des types différents pour les feuilles et les noeuds *)
type ('f,'n) arbre = F of 'f | N of ('f,'n) arbre list ;;
 




type ('f,'n) arbre = F of 'f | N of 'n * ('f,'n) arbre * ('f,'n) arbre ;;

let rec has e = function
| []-> false
| h::t-> if e =h then true else has e t;;

(* arbre binaire *)
type 'a arbre = 
 | F of 'a 
 | N of 'a * 'a arbre * 'a arbre;;

(*  arbre réduit à une feuille N _  *)
(*  arbre avec au moins un noeud interne donc de la forme N(_,g,d)*)

let rec nb_nodes a = match a with
  | F _ -> 0
  | N(_,g,d) -> 1 + nb_nodes g + nb_nodes d;;
  





let rec get_elt i l = match l with
| [] -> raise (failure "Empty list")
| h::_ when h=0-> h
| h::t ->  get_elt (i-1) t;;  

let rec dup = function
| []-> []
| h::t-> h::h::dup;;

let rec reverse l rl= 
match l with
| []-> rl
| h::t -> reverse t rl@[h];;
