let rec sigma1 = fun f a b ->
  if a > b then 0 else f a + sigma f (a + 1) b
(*
# sigma (fun x -> x) 1 174711;;
- : int = 15262054116
# sigma (fun x -> x) 1 174712;;
Stack overflow during evaluation (looping recursion?).
*)

let sigma2 = fun f a b ->
  let rec sigma_rec = fun i acc ->
    if i > b then acc else sigma_rec (i + 1) (f i + acc) in
  sigma_rec a 0
(*
# sigma (fun x -> x) 1 10000000;;
- : int = 50000005000000
*)

let rec sigma3 = fun f a b step ->
  if a > b then 0 else f a + sigma3 f (a + step) b step

let rec sigma4 = fun f a b s ->
  if a > b then 0 else f a + sigma4 f (s a) b s

let rec sigma5 = fun f a s pred ->
  let rec sigma_rec = fun i ->
    if pred i then f i + sigma_rec (s i) else 0 in
  sigma_rec a

let rec sigma6 = fun f a s pred bin e ->
  let rec sigma_rec = fun i ->
    if pred i then bin (f i) (sigma_rec (s i)) else e in
  sigma_rec a

let fact = fun n ->
  sigma6 (fun x -> x) 1 succ (fun i -> i <= n) ( * ) 1


let pi = fun epsilon ->
  let f = fun x -> 1. /. float (2 * x + 1) in
  let terme = fun x ->
    let signe = if x mod 2 = 0 then 1. else -1. in
    signe *. f x in
  4. *. sigma6 terme 0 succ (fun x -> f x > epsilon) (+.) 0.

let integrale = fun a b f dx ->
  sigma6 (fun x -> dx *. f x) a (fun x -> x +. dx) (fun x -> x <= b) (+.) 0.

