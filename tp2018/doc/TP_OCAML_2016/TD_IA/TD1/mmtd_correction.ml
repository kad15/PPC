(*tree structure*)
type 'a tree = 
  Leaf of 'a 
| Nodmin of 'a tree list 
| Nodmax of 'a tree list 
;;

(*Tree example seen in class*)
let t1= Nodmin((Leaf 10)::(Leaf max_int)::[]);;
let t2= Nodmin((Leaf 5)::(Leaf (-10))::[]);;
let t3= Nodmin((Leaf 6)::[]);;
let t4= Nodmin((Leaf 2)::(Leaf 8)::[]);;
let t5= Nodmin((Leaf (-5))::[]);;
let t6= Nodmin((Leaf 0)::[]);;
let t7= Nodmin((Leaf (-1))::(Leaf (-max_int))::(Leaf 100)::[]);;
let t8= Nodmin((Leaf 3)::(Leaf 7)::[]);;
let t9= Nodmin((Leaf (-5))::(Leaf (-3))::[]);;
  
let t10=Nodmax(t1::t2::[]);;
let t11=Nodmax(t3::[]);;
let t12=Nodmax(t4::[]);;
let t13=Nodmax(t5::t6::[]);;
let t14=Nodmax(t7::t8::[]);;
let t15=Nodmax(t9::[]);;

let t16=Nodmin(t10::t11::[]);;
let t17=Nodmin(t12::t13::[]);;
let t18=Nodmin(t14::t15::[]);;

let t=Nodmax(t16::t17::t18::[]);;


(*Reverse the tree right to left*)
let rec reverse t=
  match t with 
    Leaf x -> Leaf x
  | Nodmin l -> Nodmin (List.rev (List.map( fun x ->reverse x) l))
  | Nodmax l -> Nodmax (List.rev (List.map( fun x ->reverse x) l))
;;
    
(*Reversed tree*)
let rt =reverse t;;


let count1=ref 0

(*Write the recursive function that calculates the minimax of tree t*)
let rec minimax t =
  match t with
    Leaf x -> incr count1; x
  | Nodmin l -> List.fold_left (fun acc son -> min acc (minimax son)) max_int l
  | Nodmax l -> List.fold_left (fun acc son -> max acc (minimax son)) (-max_int) l
;;


let test_minimax () =
  count1:= 0;
  let r= minimax t in
  Printf.printf "t : res=%d, %d evaluations de feuille\n" r !count1;
  count1:=0;
  let r= minimax rt in
  Printf.printf "reverse(t) : res=%d, %d evaluations de feuille\n" r !count1;;

test_minimax ();;

let count2=ref 0;;


(*Write the recursive alpha-beta function applied to tree t*)
let rec alphabeta t alpha beta =
  match t with
    Leaf x ->
      incr count2; x
  | Nodmin lmin ->
    let rec loop s l =
      match l with
	son::ls ->
	  let new_s= min s (alphabeta son alpha s) in
	  if new_s <= alpha then new_s
	  else loop new_s ls
      | [] -> s in
    loop beta lmin
  | Nodmax lmax ->
    let rec loop s l =
      match l with
	son::ls ->
	  let new_s= max s (alphabeta son s beta) in
	  if new_s >= beta then new_s
	  else loop new_s ls
      | [] -> s in
    loop alpha lmax
;;

let test_alphabeta () =
  count2:= 0;
  let r= alphabeta t (-max_int) max_int in
  Printf.printf "t : res=%d, %d evaluations de feuille\n" r !count2;
  count2:=0;
  let r= alphabeta rt (-max_int) max_int in
  Printf.printf "reverse(t): res=%d, %d evaluations de feuille\n" r !count2;;

test_alphabeta ();;

let count3=ref 0;;
  
(*Write the recursive negamax function applied to tree t*)
let rec negamax t alpha beta =
  match t with
    Leaf x -> incr count3; x
  | Nodmin l | Nodmax l->
    let rec loop s l =
      match l with
	son::ls -> 
	  let new_s= max s (-negamax son (-beta) (-s) ) in
	  if new_s>= beta then new_s
	  else loop new_s ls
      | [] -> s in
    loop alpha l
;;

(* Version en style impératif *)

exception Sortie;;

let rec negamax t alpha beta =
  match t with
    Leaf x -> incr count3; x
  | Nodmin l | Nodmax l->
    let s= ref alpha in
    try
      List.iter
	(fun son ->
	  s:= max !s (-negamax son (-beta) (-alpha) (*- !s*) );
	  if !s>= beta then raise Sortie)
	l;
      !s
    with Sortie -> !s
;;


let test_negamax () =
  count3:= 0;
  let r= negamax t (-max_int) max_int in
  Printf.printf "t : res=%d, %d evaluations de feuille\n" r !count3;
  count3:=0;
  let r= negamax rt (-max_int) max_int in
  Printf.printf "reverse(t): res=%d, %d evaluations de feuille\n" r !count3;;
    
test_negamax ();;
      
(*Executes the three functions on tree t*)

let _ =
  count1:=0;count2:=0;count3:=0;
  let r1= (minimax t)
  and r2= (alphabeta t (-max_int) max_int)
  and r3= (negamax t (-max_int) max_int) in
  Printf.printf "%d %d\n" r1 !count1;
  Printf.printf "%d %d\n"  r2 !count2;
  Printf.printf "%d %d\n"  r3 !count3
;;


