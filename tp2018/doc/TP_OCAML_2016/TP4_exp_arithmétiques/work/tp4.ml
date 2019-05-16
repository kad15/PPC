(********************  Q1  ***********************)
type expr = 
  Float of float
 | Var of string
 | Som of expr*expr
 | Prod of expr*expr
 | Opp of expr
 | Inv of expr;;


(********************  Q2  ***********************)
let x = 1.;;
let e1 = Prod(Opp(Inv (Float 2.)), Som (Float 1.,Prod (Float 3.,Float x)));;

(********************  Q3  ***********************)
let rec eval e = match e with
   Var _ -> failwith "variable inconnue !"
 | Prod (x,y) -> eval x *. eval y 
 | Som (x,y) -> eval x +. eval y
 | Opp x -> (-1.) *. (eval x)
 | Inv x -> 1. /. (eval x)
 | Float x -> x
;;
eval e1;;

(********************  Q3  ***********************)
let rec eval2 g e = match e with
   Var x -> g x
 | Prod (x,y) -> eval2 g x *. eval2 g y 
 | Som (x,y) -> eval2 g x +. eval2 g y
 | Opp x -> (-1.) *. (eval2 g x)
 | Inv x -> 1. /. (eval2 g x)
 | Float x -> x
;;


let g v = match v with
 "x" -> 1.
 | _ -> failwith "subst variable inconnue"
;;

(********************  Q4  ***********************)


(********************  Q5  ***********************)
(*
Écrire une fonction
derive : expr -> string -> expr
qui dérive une expression par rapport à une variable donnée.
*)
let rec derive e var = 
  match e with
    Float _ -> Float 0.
  | Var v -> if v = var then Float 1. else Float 0.
  | Som(x, y) -> Som(derive x var,derive y var)
  | Prod(x,y) -> Som(Prod(derive x var, y), Prod(x, derive y var))
  | Opp x -> Opp(derive x var) 
  | Inv x -> Opp( Prod(derive x var, Inv (Prod(x,x))));;


(********************  Q6  ***********************)
(*
Écrire l’itérateur pour le type expr.
*)

let iterexp float var somme produit oppose inverse exp =
  let rec iter e = match e with
    Float x -> float x
   | Var x -> var x
   | Som (x,y) -> somme (iter x) (iter y)
   | Prod (x,y) -> produit (iter x) (iter y)
   | Opp x -> oppose (iter x)
   | Inv x -> inverse (iter x) in 
   iter exp;;

(********************  Q7  ***********************)
(*
Réécrire la fonction eval2 en utilisant l’itérateur.
*)

let eval2 e = iterexpr float var somme produit oppose inverse e

let eval2 e = iterexpr  1. (+.) ( *. ) ((-.) 0.) ((/.) 1.) e;;

(********************  Q7  ***********************)
(*
En utilisant l’itérateur, écrire une fonction
simplifie: expr -> expr
qui simplifie une expression donnée en utilisant (au moins) les règles suivantes :
∀e 0 + e = e + 0 = e, ∀e 1 ∗ e = e ∗ 1 = e, ∀e 0 ∗ e = e ∗ 0 = 0, −0 = 0
*)


(********************  Q8  ***********************)
let simplifie = 
  let float c = Float c 
  and var x = Var x
  and somme e1 e1 = 
    match (e1,e2) withe
      (Float 0., _) -> e2
   |  (_, Float 0.) -> e1
   | _ -> Som (e1,e2)
  and produit e1 e2 = 
    match (e1,e2) with 
      (Float 0.,_) -> Float 0.
    | (_, Float 0.) -> Float 0.
    | (Float 1.,_) -> e2
    | (_, Float 1.) -> e1
    | _ -> Prod (e1, e2)
   and oppose e = 
     match e with 
       Float 0. -> Float 0.
     | _ -> Opp e
   and inverse e = Inv e in
   interexpr float var somme produit oppose inverse;;
  























