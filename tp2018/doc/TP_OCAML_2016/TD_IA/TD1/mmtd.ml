(*tree structure*)
type 'a tree = 
  Leaf of 'a 
| Nodmin of 'a tree list 
| Nodmax of 'a tree list 

(*Tree example seen in class*)
let t1= Nodmin((Leaf 10)::(Leaf max_int)::[])
let t2= Nodmin((Leaf 5)::(Leaf (-10))::[])
let t3= Nodmin((Leaf 6)::[])
let t4= Nodmin((Leaf 2)::(Leaf 8)::[])
let t5= Nodmin((Leaf (-5))::[])
let t6= Nodmin((Leaf 0)::[])
let t7= Nodmin((Leaf (-1))::(Leaf (-max_int))::(Leaf 100)::[])
let t8= Nodmin((Leaf 3)::(Leaf 7)::[])
let t9= Nodmin((Leaf (-5))::(Leaf (-3))::[])
  
let t10=Nodmax(t1::t2::[])
let t11=Nodmax(t3::[])
let t12=Nodmax(t4::[])
let t13=Nodmax(t5::t6::[])
let t14=Nodmax(t7::t8::[])
let t15=Nodmax(t9::[])

let t16=Nodmin(t10::t11::[])
let t17=Nodmin(t12::t13::[])
let t18=Nodmin(t14::t15::[])

let t=Nodmax(t16::t17::t18::[])


(*Reverse the tree right to left*)
let rec reverse t=
  match t with 
    Leaf x -> Leaf x
  | Nodmin l -> Nodmin (List.rev (List.map( fun x ->reverse x) l))
  | Nodmax l -> Nodmax (List.rev (List.map( fun x ->reverse x) l))
    
(*Reversed tree*)
let rt =reverse t


let count1=ref 0
(*Write the recursive function that calculates the minimax of tree t*)
let rec minimax t =
  0


let count2=ref 0

(*Write the recursive alpha-beta function applied to tree t*)
let rec alphabeta t alpha beta =
  0


let count3=ref 0
  
(*Write the recursive negamax function applied to tree t*)
let rec negamax t alpha beta =
  0
    
      
(*Executes the three functions on tree t*)
let _ =
  Printf.printf "%d " (minimax t);
  Printf.printf "%d\n" !count1;
  Printf.printf "%d " (alphabeta t (-max_int) max_int);
  Printf.printf "%d\n" !count2;
  Printf.printf "%d " (negamax t (-max_int) max_int);
  Printf.printf "%d\n" !count3

