(** {1 Float Interval Domain Operations} *)

(** {% \paragraph{Overview} %} *)
(** This module provides functions to create and handle floating-point
   bounded interval domains. *)

(** {2 Basics} *)
type elt = Fcl_float_interval.t
(** Type of the elements of interval domains, which are themselves
   intervals, possibly reduced to a single point or with a width
   smaller than the precision required for instantiated variables. *)

type t = Fcl_float_interval.t
(** Type of interval domains. *)

val positive : t
val negative : t
val everything : t
(** Domains defined to respectively be \[[0, infinity]\],
   \[[neg_infinity, 0]\], \[[neg_infinity, infinity]\]. *)
val piI : t
(** The smallest domain enclosing pi. *)
val empty : t
(** The empty domain. *)


(** {2 Creation} *)
val interval : elt -> elt -> t
(** [interval e1 e2] returns the smallest domain enclosing [e1] and [e2]. *)
val singleton : elt -> t
(** [singleton e] returns a domain equals to [e]. *)
val intiI : int -> t
(** [intiI i] returns the point domain enclosing [i]. *)
val intsI : int -> int -> t
(** [intsI i1 i2] returns the domain [floatsI (float i1) (float i2)]. *)
val floatI : float -> t
(** [floatI f] returns the point domain enclosing [f]. *)
val floatsI : float -> float -> t
(** [floatsI f1 f2] returns a domain [d] such that [min_val d] is equal to
   [f1] and [max_val d] is equal to [f2]. *)
val float2elt : float -> elt
(** [float2elt f] returns a point element equals to \[[f,f]\]. *)



(** {2 Precision} *)
val get_precision : unit -> float
(** [get_precision ()] returns the precision under which a domain is
   considered to be bound. *)
val set_precision : float -> unit
(** [set_precision e] sets the precision under which a domain is
   considered to be bound. *)
val get_delta_ratio : unit -> float
(** [get_delta_ratio ()] returns the delta ratio, i.e the minimum ratio
   between old and new domain size for propagation to be triggered. *)
val set_delta_ratio : float -> unit
(** [set_delta_ratio ()] sets the delta ratio, i.e the minimum ratio
   between old and new domain size for propagation to be triggered. *)


(** {2 Access} *)
val fprint_elt : out_channel -> elt -> unit
val fprint : out_channel -> t -> unit
(** Printing functions for domains and their elements. *)

val is_empty : t -> bool
(** [is_empty d] returns [true] iff [d] is empty. *)
val is_bound : t -> bool
(** [is_bound d] returns true iff the width of [d] is smaller than
   [get_precision ()], and thus is bound. *)
val elt_value : t -> elt
(** [elt_value d] returns the interval corresponding to [d] iff [d]
   is bound. *)
val delta_width : t -> float
(** [delta_width d] returns
   [get_delta_ratio () *. size d +. get_precision ()]. *)
val affine_delta : t -> t -> bool
(** [affine_delta old current] returns [true] iff the delta over the
   min or max bound between [current] and [old] has changed more than
   [delta_width old]. *)
val constant_delta : t -> t -> bool
(** [affine_delta old current] returns [true] iff the delta over the
   min or max bound between [current] and [old] has changed more than
   [get_precision ()]. *)

type size = float
val size : t -> size
(** The width of a domain. *)
val dom_changed : t -> t -> bool
val min_changed : t -> t -> bool
val max_changed : t -> t -> bool
(** [dom_changed old current] [min_changed old current]
   [max_changed old current] Functions used to trigger propagation
   according to a change greater than the [delta_width] of the [old]
   domain. *)
val mem : elt -> t -> bool
(** [mem e d] returns true iff element [e] is included in domain [d]. *)
val included : t -> t -> bool
(** [included d1 d2] returns true iff domain [d1] is included
   in domain [d2]. *)
val strictly_inf : elt -> elt -> bool
(** Domains strict comparison. Returns [true] iff [max_val d1 < min_val d2]. *)
val compare_elt : elt -> elt -> int
val compare : t -> t -> int
(** Total ordering functions over elements and domains. *)
val min : t -> elt
val max : t -> elt
val min_max : t -> elt * elt
val mid : t -> elt
(** Access to the minimal, maximal and middle point elements of a domain. *)
val min_val : t -> float
(** [min_val d] returns the floating-point lower bound of [d]. *)
val max_val : t -> float
(** [max_val d] returns the floating-point upper bound of [d]. *)
val mid_val : t -> float
(** [mid_val d] returns a floating approximation of the real mean of
   [min_val d] and [max_val d]. *)
val int_bounds : t -> int * int
(** [int_bounds d] returns the bounds of the largest integer interval
   included in [d]. *)

(** {2 Refinements} *)
val remove_low : elt -> t -> t
(** [remove_low e d] returns a domain equals to [d] less all the elements
   strictly less than [e]. *)
val remove_up : elt -> t -> t
(** [remove_up e d] returns a domain equals to [d] less all the elements
   strictly greater than [e]. *)
val union : t -> t -> t
(** [union d1 d2] returns the smallest domain [d] such that [included d1 d]
   and [included d2 d] both return true. *)
val intersect : t -> t -> t
(** [intersect d1 d2] returns the smallest domain [d] such that
   [included d d1] and [included d d2] both return true. *)

(** {2 Linear and Non-linear Operations} *)

(** All operations are defined over an extended interval arithmetic with
   infinite and empty values. *)

val addIII : t -> t -> t
val subIII : t -> t -> t
val mulIII : t -> t -> t
val divIII : t -> t -> t
val powIII : t -> t -> t
val absII : t -> t
val powII : t -> int -> t
val negII : t -> t
val sqrtII : t -> t
val sqrII : t -> t
val expII : t -> t
val logII : t -> t
val acosII : t -> t 
val acoshII : t -> t
val acotII : t -> t 
val acothII : t -> t
val asinII : t -> t
val asinhII :t -> t
val atanII : t -> t
val atanhII : t -> t
val cosII : t -> t
val coshII : t -> t
val cotII : t -> t
val cothII : t -> t
val sinII : t -> t
val sinhII : t -> t
val tanII : t -> t
val tanhII : t -> t
(** Linear and non-linear operations over domains. *)
