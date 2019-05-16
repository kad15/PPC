type 'a t
exception Empty
val empty : 'a t
val take : 'a t -> 'a * 'a t
val add : 'a -> 'a t -> 'a t
val is_empty : 'a t -> bool
val size : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit
