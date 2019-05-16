val iter_until : ('a -> [`Break of 'b | `Continue]) -> 'a list -> 'b option
val iteri : (int -> 'a -> 'b) -> 'a list -> unit
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val from_to : int -> int -> int list
  (** [from_to f t = \[f..t\]] *)

val (--) : int -> int -> int list
  (** Same as from_to. [f--t = \[f..t\]] *)

val init_from_to : int -> int -> (int -> 'a) -> 'a list
  (** [init_from_to f t fn = \[fn x | x <- \[f..t\] \]] *)

val find_opt : ('a -> bool) -> 'a list -> 'a option
val find_map_opt : ('a -> 'b option) -> 'a list -> 'b option
val assoc_all : 'a -> ('a * 'b) list -> 'b list
val assoc_opt : 'a -> ('a * 'b) list -> 'b option

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val rev_filter_map : ('a -> 'b option) -> 'a list -> 'b list
val concat_map : ('a -> 'b list) -> 'a list -> 'b list
(** concatMap of Haskell. bind for the list monad. Non tail rec *)

val take : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val span : ('a -> bool) -> 'a list -> 'a list * 'a list
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val partition_map : ('a -> 'b option) -> 'a list -> 'b list * 'a list
val split_at : int -> 'a list -> 'a list * 'a list

val zip : 'a list -> 'b list -> ('a * 'b) list
(** Haskell's zip. Like List.combine but does not raise 
    when the lengthes are not equal. Tail recursive. *)

val uniq_dup : ('a -> 'a -> bool) -> 'a list -> 'a list * ('a * 'a) list
(** Filter out duplicate elements using the given equality.
    The first result list is the list of first unique occurrences,
    the second result list is the rest, the duplications removed 
    from the first result list, paired with the corresponding element
    of the first result list.
*)

val unique : 'a list -> 'a list (** Haskell's nub *)

val is_unique : ('a -> 'b) -> 'a list -> ('a * 'a) option
  (** Check the list is a unique list or not, wrt the key function. 
      If not, it returns a dupe example. 
      
      [is_unique fst [(1,2); (2,3); (4,5)] = None]
      [is_unique fst [(1,2); (2,3); (2,5)] = Some ( (2,3), (2,5) ) ]
  *)

val intersperse : 'a -> 'a list -> 'a list

val last : 'a list -> 'a
  (** raises Failure when the argument is [].
      [last [1;2;3] = 3]
  *)

val scani_left : 
  (int -> 'a -> 'b -> [< `Continue of 'a | `Stop of 'a ]) 
  -> 'a -> 'b list -> 'a
  (** [foldl] but stoppable *)

val fold_left1 : ('a -> 'a -> 'a) -> 'a list -> 'a
(** List must be non-empty. 
    Otherwise, it raises [Invalid_argment "fold_left1"]. *)

val sum : int list -> int

val group : ('a -> 'a -> bool) -> 'a list -> 'a list list

val sort_then_group : ('a -> 'a -> int) -> 'a list -> 'a list list

val splits_by : int -> 'a list -> 'a list list
(** Split a list into sub-lists of the fixed length *)

val accum  : 'a list ref -> 'a -> unit
(** [accum xsref x] is equivalent with [xsref := x :: !xsref] *)

val (+::=) : 'a list ref -> 'a -> unit
(** Same as [accum] *)

module Infix : sig
  val (--) : int -> int -> int list
  (** [same as from_to. [f--t = [f..t]] ] *)

  val (+::=) : 'a list ref -> 'a -> unit
  (** Same as [accum] *)
end

module Pervasives : sig
  val (--) : int -> int -> int list
  (** [same as from_to. [f--t = [f..t]] ] *)

  val (+::=) : 'a list ref -> 'a -> unit
  (** Same as [accum] *)
end


module TR : sig
  val map : ('a -> 'b) -> 'a list -> 'b list
  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
  val rev_concat_map : ('a -> 'b list) -> 'a list -> 'b list
end
