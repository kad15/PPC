type arbre = N of arbre list;;


type ('f,'a) arbre =
  | F of 'f
  | N of 'a * ('f,'a) arbre * ('f,'a) arbre;;

(* repr de l'équation x + yz*)

N("+", F("x"), N("*",F("y"),F("z")));;;

(* type arbre binaire entier ie dont tous les noeud internes sont d'arité 2*)
type 'a arbre =
  | F of 'a
  | N of 'a * 'a arbre * 'a arbre;;

arbre réduit à une feuille F _
arbre qui contient au moins un noeud interne :
de la forme N(_,g,d)

fonctions de la forme :

let rec f x = match x with
  | F _ -> ...
  | N(_,_,_) -> ...;;

(*nb noeuds internes *)
let rec  nb_noeuds a = match a with
  | F _ -> 0
  | N(_,g,d) -> 1 + nb_noeuds g + nb_noeuds d;;

(* nb feuilles *)
let rec nb_feuilles a = match a with 
  | F _ -> 1
  | N(_,g,d) -> nb_feuilles g + nb_feuilles d;;


(* hauteur *)
let rec hauteur a = match a with
 | F _ -> 0
 | N(_,g,d) -> 1 + max (hauteur g) (hauteur d);;


(*miroir d'un arbre *)
let rec miroir a = match a with 
 | F(x) -> F(x)
 | N(x,g,d) -> N(x, miroir d, miroir g);;

let t = N(1, F 2, N(3, F 4, F 5));;



(* parcours en profondeur *)

(*modèle inutile*)
let rec visite a = match a with
 | F _ -> ()
 | N(_,g,d) -> visite g; visite d;;

let t = N(1, N(2, F 4, N(5, F 8, F 9)), N(3, F 6, F 7));;
(* parcours prof préfixe*)
let rec parcours_prefixe a = match a with 
 | F(x) -> print_int x
 | N(x,g,d) -> print_int x; parcours_prefixe g ; parcours_prefixe d;;


let rec parcours_infixe a = match a with 
 | F(x) -> print_int x
 | N(x,g,d) ->  parcours_infixe g ; print_int x;parcours_infixe d;;

let rec parcours_suffixe a = match a with 
 | F(x) -> print_int x
 | N(x,g,d) ->  parcours_suffixe g ;parcours_suffixe d; print_int x;;




(*  parcours en largeur *)

let rec liste_racines la = match la with
 | [] ->  []
 | (F x)::l -> x::(liste_racines l)
 | N(x,g,d)::l -> x::(liste_racines l);;



let rec liste_fils la = match la with 
 | []->[]
 | (F x)::l -> liste_fils l
 | N(x,g,d)::l-> g::(d::(liste_fils l));;


let parcours_largeur a = 
 let rec aux l = match l with
   | []->[]
   | _ -> (liste_racines l)@(aux (liste_fils l))
 in aux [a];; 

(*******************************************************************)
(*ARBRE DE RECHERCHE *)
type 'a arbre = Vide | N of 'a * 'a arbre * 'a arbre | F of 'a;;
let abr = N(9, N(5,N(3,F 1, F 4), N(8, F 7, Vide)), N(13,F 10, N(15,Vide, F 17)));;

let rec parcours_infixe a = match a with 
 | F(x) -> print_int x
 | Vide -> ()
 | N(x,g,d) ->  parcours_infixe g ; print_int x;parcours_infixe d;;


parcours_infixe abr;;

