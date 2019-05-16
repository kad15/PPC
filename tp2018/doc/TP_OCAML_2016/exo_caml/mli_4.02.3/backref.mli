type 'a t

val create : string -> 'a t
val set : 'a t -> 'a -> unit
val get : 'a t -> 'a
