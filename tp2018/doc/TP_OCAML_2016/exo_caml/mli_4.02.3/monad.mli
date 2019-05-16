(** Monad implementation. See [Monad_intf] for more details. *)

open Monad_intf

(** Generic monad type *)
module Generic : sig
  type 'a t
       
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val ( >>| ) : ('a -> 'b) -> 'a t -> 'b t
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val ( ^<$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( /<*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val void : 'a t -> unit t
  val seq : 'a t list -> 'a list t
  val seq_ : unit t list -> unit t
  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  val mapM_ : ('a -> unit t) -> 'a list -> unit t
  val for_ : int -> int -> (int -> unit t) -> unit t
  val iteri : (int -> 'a -> unit t) -> 'a list -> unit t
  val join : 'a t t -> 'a t
end

module Make(M : S) : sig
  include T with type 'a t := 'a M.t
  val run : 'a Generic.t -> 'a M.t
end

module Make2(M : S2) : T2 with type ('a, 'z) t := ('a, 'z) M.t
