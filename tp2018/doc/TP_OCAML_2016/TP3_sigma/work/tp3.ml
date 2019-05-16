(******** Q1 ************)
let rec sigma1 f a b = 
  if a > b then 0 else f a + sigma1 f (a+1) b;;

let s = sigma1 (fun x -> x) 1 200_0 in
Printf.printf "sigma1 : %d\n" s;;


(******** Q2 ************)
let sigma_term f a b =
  let rec aux f a b acc = 
     if a > b then acc  else aux f (a+1) b (acc + f a)
     in aux f a b 0;;

let s = sigma_term (fun x -> x) 1 200_000 in
Printf.printf "sigma_term : %d\n" s;;

(******** Q3 ************)
let sigma3 f a b i =
  let rec aux f a b acc = 
     if a > b then acc  else aux f (a+i) b (acc + f a)
     in aux f a b 0;;

let i = 2 in
let s = sigma3 (fun x -> x) 1 10 i in
Printf.printf "sigma2: increment = %d ; res = %d\n" s i;;

(******** Q4 ************)
let rec sigma4 f a b s =
  if a > b then 0 else f a + sigma4 f (s a) b s;;


(******** Q5 ************)
let rec sigma5 f a s pred =
  if pred a then f a + sigma5 f (s a) s pred else 0;;

(******** Q6 ************)
let rec sigma6 f a s pred bin e =
    if pred a then bin (f a) (sigma6 f (s a) s pred bin e) else e;;

(******** Q7 ************)
let facto n = sigma6 (fun x->x) n (fun x->x-1) (fun x -> x>0) (fun x y ->x*y) 1;;



(******** Q8 ************)
let pi epsilon =
  let f x = 1. /. float (2 * x + 1) in
  let terme x =
    let signe = if x mod 2 = 0 then 1. else -1. in
    signe *. f x in
  4. *. sigma6 terme 0 succ (fun x -> f x > epsilon) (+.) 0.



(******** Q9 ************)
let integrale a b f dx =
  sigma6 (fun x -> dx *. f x) a (fun x -> x +. dx) (fun x -> x <= b) (+.) 0.





