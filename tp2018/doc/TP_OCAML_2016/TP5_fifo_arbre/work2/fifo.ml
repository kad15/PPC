(*1*)
type 'a t = 'a list * 'a list;;

(*2*)
exception Empty;;

(*3*)
let empty = ([],[]);;

(*4*)
let is_empty fifo = 
  fifo = empty;;
 
(*5*)
let add elt (l1,l2) = (elt::l1,l2);;

(*6*)
let rec take fifo = 
    match fifo with
   | ([],[]) -> raise Empty
   | (fi,[]) -> take ([],List.rev fi)
   | (fi,h::t) -> (h,(fi,t));; 


(*7*)
(*
module type Mli =
  sig
type 'a t
exception Empty
val empty : 'a list * 'b list
val is_empty : 'a list * 'b list -> bool
val add : 'a -> 'a list * 'b -> 'a list * 'b
val take : 'a list * 'a list -> 'a * ('a list * 'a list)
  end
*)
