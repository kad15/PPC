(* I - FIle d'attente '*)

(* 1 *)
type 'a t = 'a list * 'a list ;;

(* 2 *)
exception Empty;;

(* 3 *)
let empty =  ([],[]);;

(* 4 *)
let is_empty q = q = empty;;
  

(* 5 *)
(* Définir la fonction add d’ajout d’un élément dans la file qui prend un élément et une file
en paramètres et renvoie la nouvelle file. *)

(* ma solution
let add (x:'a) (file:'a t) :'a t= 
  match file with 
    (entree,sortie) -> (x::entree,sortie);; *)


let add x (front, back) = (x::front, back)
   

(* 6 *)
(**  Définir la fonction take qui prend une file en paramètre et renvoie l’élément en tête de
file et la nouvelle file. On lèvera l’exception définie précédemment si la file est vide.  ***)
let rec take q =
  match q with
    ([], []) -> raise Empty
  | (front, []) -> take ([], List.rev front)
  | (front, x :: back) -> (x, (front, back))

(* 7 *)
(* Écrire le fichier d’interface .mli correspondant en cachant l’implémentation du type des files (type abstrait). *)


