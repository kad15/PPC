include Monad_intf.T with type 'a t = 'a option

val default : 'a t -> (unit -> 'a) -> 'a
(** Haskell's fromMaybe.

    Ex.
    [default (Some v) f = v]
    [default None f = f ()]

    The default value is provided not as a direct value of type ['a] 
    but as a thunk [(unit -> 'a)], since such a value may be created
    dynamically.
*)

val (//) : 'a t -> 'a -> 'a
(** Same as [default] but the default value is always evaluated *)

val (>>=!) : 'a t -> (unit -> 'a t) -> 'a t  
(** bindE. *)

val catch : (fail:(unit -> 'exn) -> 'a) -> 'a option
val catch_exn : (unit -> 'a) -> 'a option

val to_result : 'a option -> ('a, [> `None]) Result.t

val from_Some : 'a option -> 'a
(** may raise [Invalid_argument] *)

val format : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

val iter : ('a -> unit) -> 'a option -> unit

module Pervasives : sig 
  val from_Some : 'a option -> 'a
  (** may raise [Invalid_argument] *)

  val (//) : 'a t -> 'a -> 'a
  (** Same as [default] but the default value is always evaluated *)
end

