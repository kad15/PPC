type 'a t
exception Empty
val empty : 'a list * 'b list
val is_empty : 'a list * 'b list -> bool
val add : 'a -> 'a list * 'b -> 'a list * 'b
val take : 'a list * 'a list -> 'a * ('a list * 'a list)
