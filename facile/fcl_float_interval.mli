(** {1 Continuous Interval Operations} *)

(** Implementation of floating-point bounded intervals. *)

type t
(** The interval type. *)
 

(** {2 Special Values} *)

val everything : t 
(** The interval containing everything. *)
val p_i : float
(** The positive infinite floating point value. *)
val m_i : float
(** The negative infinite floating point value. *)
val empty : t
(** The empty interval. *)
val piI : t
(** The smallest floating point bounded interval containing the real value pi. *)

(** {2 Creation} *)
val intiI : int -> t
(** [intiI x] returns the point interval containing the value [float x]. *)
val intsI : int -> int -> t
(** [intsI x y] returns the interval [floatsI (float x) (float y)]. *)
val floatI : float -> t
(** [floatI x] returns the point interval containing the value [x]. *)
val floatsI : float -> float -> t
(** [floatsI x y] returns an interval [i] such that [min_val i] is equal to [x] and [max_val i] is equal to [y]. *)


(** {2 Access} *)

val is_empty : t -> bool
(** [is_empty i] returns [true] if [i] is the [empty] interval, else it returns [false]. *)
val is_point : t -> bool
(** [is_point i] returns [true] if [i] contains only one value, meaning that the lower bound of [i] is equal to its upper bound. *)
val size : t -> float
(** [size i] returns a floating point approximation of the real distance between the lower bound of [i] and its upper bound. *)
val compare : t -> t -> int
(** [compare i1 i2] returns [compare (max_val i1) (max_val i2)] if 
[compare (min_val i1) (min_val i2)] is equal to 0 else [compare (min_val i1) (min_val i2)]. This is a total ordering function. *) 
val fprint : out_channel -> t -> unit 
(** [fprint channel i] prints a representation of the interval [i] on the out channel [channel]. *)
val included : t -> t -> bool
(** [included i1 i2] returns [true] if [i1] is included in or equal to [i2] and [false] otherwise. *)
val min_val : t -> float
(** [min_val i] returns the floating-point lower bound of [i]. *)
val max_val : t -> float
(** [max_val i] returns the floating-point upper bound of [i]. *)
val strictly_inf : t -> t -> bool
(** [strictly_inf i1 i2] returns [true] iff [max_val i1] < [min_val i2]. *)
val mid_val : t -> float
(** [mid_val i] returns a floating approximation of the real mean of [min_val i] and [max_val i]. *)


(** {2 Set Operations} *)

val union : t -> t -> t
(** [union i1 i2] returns the smallest interval [i] such that [included i1 i]
   and [included i2 i] both return true. *)
val intersect : t -> t -> t
(** [intersect i1 i2] returns the largest interval [i] such that [included i i1] and [included i i2] both return true. *)
val min : t -> t
(** [min i] returns the point interval containing [min_val i]. *)
val max : t -> t
(** [max i] returns the point interval containing [max_val i]. *)
val mid : t -> t
(** [mid i] returns the point interval containing [mid_val i]. *)
val remove_low : t -> t -> t
(** [remove_low i_low i] returns [i_up], the largest interval included in [i] such that [min_val i_low] <= [min_val i_up]. *)
val remove_up : t -> t -> t
(** [remove_up i_up i] returns [i_low], the largest interval included in [i] such that [max_val i_low] <= [max_val i_up]. *)


(** {2 Interval Arithmetic Operations} *)

(** All operations are defined over an extended interval arithmetic with infinite and empty values. *)

val addIII : t -> t -> t
val subIII : t -> t -> t
val mulIII : t -> t -> t
val divIII : t -> t -> t
(** The addition, substraction, multiplication and division operators over intervals. *)

val absII : t -> t
(** The absolute value operation. *)
val powIII : t -> t -> t
(** The exponentiation operator. *)
val powII : t -> int -> t
(** The exponentiation operator to an integer value. *)
val negII :t -> t
(** The unary negation operator. *)
val sqrtII :t -> t
(** The square root operator. *)
val sqrII : t -> t
(** The square operator (may be more precise than the exponentiation operators). *)
val expII : t -> t
(** The exponential operator. *)
val logII : t -> t
(** The natural logarithm oprator. *)
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
(** The usual trigonometric and hyperbolic operators over intervals. *)
