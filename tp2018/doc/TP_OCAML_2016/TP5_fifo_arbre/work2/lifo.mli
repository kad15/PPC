type 'a t = { stack : 'a list; size : int; }
exception Empty
val empty : 'a t
val is_empty : 'a t -> bool
val add : 'a -> 'a t -> 'a t
val take : 'a t -> 'a * 'a t
val size : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit
