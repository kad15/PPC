type expr = 
  Float of float
 | Var of string
 | Som of expr*expr
 | Prod of expr*expr
 | Opp of expr
 | Inv of expr;;


let iterexpr float var somme produit oppose inverse exp =
  let rec iter e = match e with
    Float x -> float x
   | Var x -> var x
   | Som (x,y) -> somme (iter x) (iter y)
   | Prod (x,y) -> produit (iter x) (iter y)
   | Opp x -> oppose (iter x)
   | Inv x -> inverse (iter x) in 
   iter exp;;

let x = 1.;;
let e1 = Prod(Opp(Inv (Float 2.)), Som (Float 1.,Prod (Float 3.,Float x)));;

let e2 = Prod(Opp(Inv (Float 2.)), Som (Float 1.,Prod (Float 3.,Float x)));;




let simplifie = 
  let float c = Float c 
  and var x = Var x
  and somme e1 e1 = 
    match e1,e2 with
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
   iterexpr float var somme produit oppose inverse;;
