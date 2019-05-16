include Monad_intf.T2 with type ('a,'error) t = [`Ok of 'a | `Error of 'error]

val fail : 'a -> [> `Error of 'a ]

val catch : (fail:('error -> 'exn) -> 'a) -> [> ('a, 'error) t]
(** [catch f] runs [f ~fail]. 

    If [f ~fail] returns a value [v] then [catch f] returns [`Ok v].

    If [f ~fail] calls [fail e], then the execution of the [f ~fail] immediately
    exists and [catch f] returns [`Error e]

    Any exception raised in [f ~fail] is not caught.
*)

val catch_exn : (unit -> 'a) -> [> ('a, exn) t ]
(** [catch_exn f] runs [f ()]. 

    If [f ()] returns a value [v] then [catch f] returns [`Ok v].

    If an exception [exn] raised in [f ()], [catch f] returns [`Error exn].
*)

exception Error

val from_Ok : [< ('a, 'error) t] -> 'a
(** Haskell's fromJust *)

val result : ('a -> 'b) -> ('c -> 'b) -> [< ('a, 'c) t] -> 'b
(** Haskell's either *)

val map_error : ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t

val (>>=!) : ('a, 'e) t -> ('e -> ('a, 'e2) t) -> ('a, 'e2) t  
(** bindE *)

val (>>|!) : ('a, 'e) t -> ('e -> 'e2) -> ('a, 'e2) t  
(** mapE *)

val at_Error : ('err -> 'a) -> [< ('a, 'err) t] -> 'a
(** [at_Error = result id] *)

module Pervasives : sig
  val ok : 'a -> [> `Ok of 'a ]
  val ng : 'a -> [> `Error of 'a ]
  (** No Good *)

  val from_Ok : [< ('a, 'error) t] -> 'a

  val result : ('a -> 'b) -> ('c -> 'b) -> [< ('a, 'c) t] -> 'b
  (** Haskell's either *)

  val at_Error : ('err -> 'a) -> [< ('a, 'err) t] -> 'a
  (** [at_Error = result id] *)
end

