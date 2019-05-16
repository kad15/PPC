(** This module is an interface between Ocaml and the C++ interval library {e filib++}. 
It provides an [interval] type and functions dealing with intervals. 

Most functions are just the Ocaml counterpart of a 
{e filib++} function and therefore are not documented.

Whenever they are available, the filib++ global 
functions are used instead of methods.
Most of the functions which are not testing functions 
returning a [bool] are named as their filib++ equivalents 
with a typing suffix. For example, [sinII] 
has the type [interval -> interval] and stands for the filib++ 
function [sin].
 
It uses the {e IEEE 754} values [infinity], [neg_infinity] 
and [nan] as defined in [Pervasives]. *) 



(** The type of an interval *)
type interval


(**  {2 Values} *)


(** The interval containing everything. *)
val everything : interval

(** The empty interval. *)
val empty : interval

(** An interval containing the real value [pi]. *)
val piI : interval

(** An interval containing the real value [0].  *)
val zeroI : interval

(** An interval containing the real value [1].  *)
val oneI : interval

(** The positive infinite interval. *)
val neg_inftyI : interval

(** The negative infinite interval. *)
val pos_inftyI : interval


(**  {2 Creation} *)


(** For the creation functions expecting two arguments [x] and [y],
   it is supposed that [x <= y].

   If that is not the case, see the filib++ documentation 
   about the behaviour of {v interval(N const & a,N const & b) v}. *)
(** same as {!Interval.floatsI}. *)
external create : float -> float -> interval = "create_"

(** [intI x] returns an interval containing [x]. *)
val intI : int -> interval

(** [intsI x y] returns an interval containing both [x] and [y]. *)
val intsI : int -> int -> interval

(** [floatI x] returns an interval containing [x]. *)
val floatI : float -> interval

(** [floatsI x y] returns an interval containing both [x] and [y].*)
external floatsI : float -> float -> interval = "create_"


(** {2 Arithmetic Operators} *)


(** The OCaml function standing for the filib++ operator [+]. *)
external addIII : interval -> interval -> interval = "addIII_"

(** The OCaml function standing for the filib++ binary operator [-]. *)
external subIII : interval -> interval -> interval = "subIII_"

(** The OCaml function standing for the filib++ operator [*]. *)
external mulIII : interval -> interval -> interval = "mulIII_"

(** The OCaml function standing for the filib++ operator [/]. *)
external divIII : interval -> interval -> interval = "divIII_"

(** The OCaml function standing for the filib++ unary operator [-]. *)
external negII : interval -> interval = "negII_"


(** {2 Access and Information} *)

(** [compare x y] returns [compare (inf xi) (inf yi)] if
   [compare (inf xi) (inf yi) <> 0] else it returns
   [compare (sup xi) (sup yi)]. *)
val compare : interval -> interval -> int

external is_empty : interval -> bool = "is_empty_"
external is_point : interval -> bool = "is_point_"
external mid : interval -> float = "mid_"
external inf : interval -> float = "inf_"
external sup : interval -> float = "sup_"

(** [inf_sup x] is equivalent to [fun x -> (inf x, sup x)] but may be
a little bit quicker. *)
external inf_sup : interval -> float * float = "inf_sup_"
external absII : interval -> interval = "absII_"
external diam : interval -> bool = "diam_"
external rel_diam : interval -> bool = "relDiam_"
external rad : interval -> bool = "rad_"
external mig : interval -> bool = "mig_"
external mag : interval -> bool = "mag_"
external dist : interval -> bool = "dist_"

(** The OCaml equivalent of the filib++ [in] function. *)
external is_in : float -> interval -> bool = "in_"


(** {2 Set Theoretic Functions} *)

external iminIII : interval -> interval -> interval = "iminIII_"
external imaxIII : interval -> interval -> interval = "imaxIII_"
external intersectIII : interval -> interval -> interval = "intersectIII_"
external hullIII : interval -> interval -> interval = "hullIII_"
external disjoint : interval -> interval -> bool = "disjoint_"
external interior : interval -> interval -> bool = "interior_"
external proper_subset : interval -> interval -> bool = "proper_subset_"
external subset : interval -> interval -> bool = "subset_"
external superset : interval -> interval -> bool = "superset_"
external proper_superset : interval -> interval -> bool = "proper_superset_"

(** {2 Interval Relational Functions} *)


(** {4 Set Relations} *)

external seq : interval -> interval -> bool = "seq_"
external sne : interval -> interval -> bool = "sne_"
external sge : interval -> interval -> bool = "sge_"
external sgt : interval -> interval -> bool = "sgt_"
external sle : interval -> interval -> bool = "sle_"
external slt : interval -> interval -> bool = "slt_"

(** {4 Certainly Relations} *)

external ceq : interval -> interval -> bool = "ceq_"
external cne : interval -> interval -> bool = "cne_"
external cge : interval -> interval -> bool = "cge_"
external cgt : interval -> interval -> bool = "cgt_"
external cle : interval -> interval -> bool = "cle_"
external clt : interval -> interval -> bool = "clt_"

(** {4 Possibly Relations} *)

external peq : interval -> interval -> bool = "peq_"
external pne : interval -> interval -> bool = "pne_"
external pge : interval -> interval -> bool = "pge_"
external pgt : interval -> interval -> bool = "pgt_"
external ple : interval -> interval -> bool = "ple_"
external plt : interval -> interval -> bool = "plt_"

(** {2 Elementary Functions} *)

external acosII : interval -> interval = "acosII_"
external acoshII : interval -> interval = "acoshII_"
external acotII : interval -> interval = "acotII_"
external acothII : interval -> interval = "acothII_"
external asinII : interval -> interval = "asinII_"
external asinhII :interval -> interval = "asinhII_"
external atanII : interval -> interval = "atanII_"
external atanhII : interval -> interval = "atanhII_"
external cosII : interval -> interval = "cosII_"
external coshII : interval -> interval = "coshII_"
external cotII : interval -> interval = "cotII_"
external cothII : interval -> interval = "cothII_"
external expII : interval -> interval = "expII_"
external exp10II : interval -> interval = "exp10II_"
external exp2II : interval -> interval = "exp2II_"
external logII : interval -> interval = "logII_"
external log10II : interval -> interval = "log10II_"
external log2II : interval -> interval = "log2II_"
external log1pII : interval -> interval = "log1pII_"
external powIII: interval -> interval -> interval = "powIII_"
external powII : interval -> int-> interval = "powII_"
external sinII : interval -> interval = "sinII_"
external sinhII : interval -> interval = "sinhII_"
external sqrtII : interval -> interval = "sqrtII_"
external sqrII : interval -> interval = "sqrII_"
external tanII : interval -> interval = "tanII_"
external tanhII : interval -> interval = "tanhII_"

(** {2 Output} *)

val sprint : interval -> string
 
val fprint: out_channel -> interval -> unit
