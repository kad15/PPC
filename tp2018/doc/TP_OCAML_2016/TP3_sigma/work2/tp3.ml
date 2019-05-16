open Printf;;
(* 1 *)

let rec sigma1 f a b = 
match a with 
  | a when a>b -> 0
  | _ -> f(a) + sigma1 f (a+1) b;;

printf "sigma1 : %d\n" (sigma1 (fun x -> x) 1 10);;

(* 2 *)
let sigma2 f a b =
  let rec aux i acc = 
    match i with
   | i when i>b -> acc
   | _ -> aux (i+1) (acc + f i) in
   aux a 0;;

printf "sigma2 : %d\n" (sigma2 (fun x -> x) 1 10);;

let sigma2 = fun f a b ->
  let rec sigma_rec = fun i acc ->
    if i > b then acc else sigma_rec (i + 1) (f i + acc) in
  sigma_rec a 0


(* 3 *)
let rec sigma3 f a b step = 
match a with 
  | a when a > b -> 0
  | _ -> f(a) + sigma3 f (a+step) b step;;

printf "sigma3 : %d\n" (sigma3 (fun x -> x) 1 10 2);;

let rec sigma3 = fun f a b step ->
  if a > b then 0 else f a + sigma3 f (a + step) b step;;

(* 4 
Généraliser en prenant en paramètre une fonction s qui calcule l’argument suivant :
f (a) + f (s(a)) + f (s(s(a))) + · · · + f (b)
*)
let rec sigma4 f a b s =
  if a > b then 0 else f a + sigma4 f (s a) b s;;

let f y = y in
let s x = x+1 in
let a = 1 and b = 10 in
printf "sigma4 : %d\n" (sigma4 f a b s);;


(* 5 
Généraliser en prenant en paramètre une condition quelconque pred pour les éléments :
f (a) + f (s(a)) + · · · + f (x) où x est le dernier argument tel que pred(x) soit vrai (i.e. pred est un prédicat).
*)

let sigma5 f a s pred =
  let rec aux i = 
    if pred i then f i + aux (s i) else 0
  in aux a;;

(*
let sigma5 = fun f a s pred ->
  let rec sigma_rec = fun i ->
    if pred i then f i + sigma_rec (s i) else 0 in
  sigma_rec a;;
*)
let f y = y in
let s x = x + 1 in
let a = 1 in
let pred z = z <= 10 in
printf "sigma5 : %d\n" (sigma5 f a s pred);;


(* 6 
Généraliser en permettant le remplacement de la somme par n’importe quelle fonction
binaire (possédant un élément neutre à prendre également en paramètre).
*)
let sigma6 f a s pred bin e =
  let rec aux i =
    if pred i then bin (f i) (aux (s i)) else e in
  aux a;;
(* 7 
Écrire la fonction factorielle à l’aide de la fonction sigma
*)
let fact n = 
  sigma6 (fun x -> x) n (fun x -> x-1) (fun x -> x>0) ( * ) 1;;

let n = 2 in
printf "%d! = %d\n" n (fact n);;

(* 8 *)
let f i = 
    let p = (2.*. float i +. 1.) in
    let sign = (-. 1.)** float i in
      sign *. (1. /. p );;
let epsilon = 1e-5;;
let pred n = abs_float(f n)  > epsilon;;

(*sigma6 f a s pred bin e *) 
let pi = 4. *. (sigma6 f 0 succ pred (+.) 0.) ;;

printf "question 8 : pi = %f\n" pi;;


(* 9 

Écrire une fonction integrale qui calcule une approximation de
intégrale de a à b de f (x)dx en prenant pour dx un « petit » flottant.
*)


let integrale f a b = 
 let dx = 1e-2 in 
   let s = sigma6 (fun x -> f x) a (fun x -> x +. dx) (fun x -> x < b) (+.) 0. in
     dx *. s ;;
printf "%f\n" (integrale (fun x-> x**2.) 0. 1.);; 

