(*1*)
type expr = 
 | Float of float
 | Var of string
 | Som of expr * expr
 | Prod of expr * expr
 | Opp of expr
 | Inv of expr;;

(*2*)
let x = "x";;
let e1 = Prod (Float 3., Var x);;
let e2 = Som(Float 1.,e1);;
let moinsundemi = Opp(Inv(Float 2.));;
let e = Prod (moinsundemi, e2);;

let e'= Prod (Opp (Inv (Float 2.)), Som (Float 1., Prod (Float 3., Var "x")));;


(*3
Écrire une fonction
eval : expr -> float
permettant d’évaluer une expression ne contenant pas de variable. Si une variable est
rencontrée, on déclenchera une erreur avec la fonction failwith 1 .
*)
let rec eval expr = 
  match expr with 
  | Var _ -> failwith "erreur presence d'une variable !"
  | Float x -> x
  | Som (e1,e2) -> eval e1 +. eval e2
  | Prod (e1,e2) -> eval e1 *. eval e2 
  | Opp e -> (-1.)*. (eval e)
  | Inv e -> 1. /. eval e

(* 4
Modifier (en la recopiant) la fonction précédente pour écrire une fonction
eval2 : (string -> float) -> expr -> float
prenant en argument une fonction d’évaluation pour les variables (une substitution) et
une expression et évaluant cette dernière pour cette substitution.
*)

let rec eval2 subst expr = 
  match expr with 
  | Var v -> subst v  
  | Float x -> x
  | Som (e1,e2) -> eval2 subst e1 +. eval2 subst e2
  | Prod (e1,e2) -> eval2 subst e1 *. eval2 subst e2 
  | Opp e -> (-1.)*. (eval2 subst e)
  | Inv e -> 1. /. (eval2 subst e);;

let subst v = 
  match v with
  | "x" -> 1.
  |  _  -> failwith "subst: variable inconnue";;


(*5 derive : expr -> string -> expr *)
let rec derive e var = 
  match e with
  | Float _ -> Float 0.
  | Var v -> if v = var then Float 1. else Float 0.
  | Som (e1,e2) -> Som(derive e1 var,derive e2 var)
  | Prod (e1,e2) -> Som(Prod(derive e1 var,e2),Prod(derive e2 var,e1)) 
  | Opp x -> Opp (derive x var)
  | Inv x -> Opp (Prod (derive x var, Inv (Prod (x, x))));;




(*6  Écrire l’itérateur pour le type expr.  ('a -> unit) -> expr -> unit *)

let iterexpr float var somme produit oppose inverse ex = 
 let rec iter e =
   match e with
     Float x -> float x
   | Var x -> var x
   | Som (x, y) -> somme (iter x) (iter y)
   | Prod (x, y) -> produit (iter x) (iter y)
   | Opp x -> oppose (iter x)
   | Inv x -> inverse (iter x) in
   iter ex;;



let rec eval2 subst expr = 
  match expr with 
  | Var v -> subst v  
  | Float x -> x
  | Som (e1,e2) -> eval2 subst e1 +. eval2 subst e2
  | Prod (e1,e2) -> eval2 subst e1 *. eval2 subst e2 
  | Opp e -> (-1.)*. (eval2 subst e)
  | Inv e -> 1. /. (eval2 subst e);;
(* 7. Réécrire la fonction eval2 en utilisant l’itérateur.*)

let eval3 subst = 
  iterexpr (fun x -> x) subst ( +. ) ( *. ) (fun x -> (-.1.)*.x) (fun x -> 1. /. x);;
 
let eval2 subst = iterexpr (fun x -> x) subst (+.) ( *. ) ((-.) 0.) ((/.) 1.);;


(* 8 En utilisant l’itérateur, écrire une fonction
simplifie: expr -> expr
qui simplifie une expression donnée en utilisant (au moins) les règles suivantes :
∀e 0 + e = e + 0 = e, ∀e 1 ∗ e = e ∗ 1 = e, ∀e 0 ∗ e = e ∗ 0 = 0, −0 = 0*)

let simplifie =
 let float c = Float c
   and var x = Var x

   and somme  e1 e2 =
  match (e1, e2) with
(Float 0., _) -> e2
| (_, Float 0.) -> e1
| _ -> Som (e1, e2)

 and produit e1 e2 =
match (e1, e2) with
(Float 0., _) -> Float 0.
| (_, Float 0.) -> Float 0.
| (Float 1., _) -> e2
| (_, Float 1.) -> e1
| _ -> Prod (e1, e2)
 and oppose e = match e with Float 0. -> Float 0. | _ -> Opp e
 and inverse e = Inv e in
 iterexpr float var somme produit oppose inverse;;

simplifie (derive e "x");;
