(* Unbalanced Binary Tree *)
    
type 'a tree =
  Node of 'a * 'a tree * 'a tree
  | Leaf;;

(* e.g Node (5, Leaf, Leaf) *)

(* val insert : ’a -> ’a btree -> ’a btree = <fun> *)
let insert x s = Node (x, Leaf, s);;

let rec make_binary_tree = function l ->
  let empty = Leaf in
    match l with
    [] -> empty
    | x :: l -> insert x (make_binary_tree l);;

(* Use recursion to find if x is an element of a binary tree *)
let rec mem x = function
  Leaf -> false
  | Node (y, left, right) ->
    x = y || mem x left || mem x right
;;

(* 
val s : int tree = Node (1, Leaf, Node (2, Leaf, Node (3, Leaf, Leaf)))
# mem 2 s;;
- : bool = true
*)

(* Ordered binary search tree *)
(*  
type 'a tree =
  Node of 'a * 'a tree * 'a tree
  | Leaf;;
*)
(* Make ordered binary tree *)
let rec insert_ordered x = function
    Leaf -> Node (x, Leaf, Leaf)
  | Node (y, left, right) as node ->
    if x < y then
      Node (y, insert_ordered x left, right)
    else if x > y then
      Node (y, left, insert_ordered x right)
    else
      node;;

(* Make a binary search tree aka ordered or sorted binary tree *) 
let rec make_ordered_binary_tree = function l ->
  let empty = Leaf in
    match l with
      [] -> empty
      | x :: xs -> insert_ordered x (make_ordered_binary_tree xs);;

(* Use recursion to find if x is an element of a binary tree *)
let rec mem x = function
    Leaf -> false
  | Node (y, left, right) ->
x = y || (x < y && mem x left) || (x > y && mem y right);;
