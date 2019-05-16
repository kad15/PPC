(** {1 Arithmetic Expressions and Constraints over intervals}
 *)

(** {% \paragraph{Overview} %} *)
(** This module provides functions and operators to build arithmetic
   expressions with interval values and state (in/dis)equation
   constraints on them. *)

(** {2 Basics} *)

type t
(** Type of arithmetic expressions over
   variables of type [FloatVar.t], [FloatDomain.t] and integers.  *)

(*** Conversion *)
val f2fe : float -> t
(** [f2fe f] returns an expression which evaluates to [f]. *)
val dom2fe : Fcl_float_domain.t -> t
(** [dom2fe d] returns an expression which evaluates to [d]. *)
val fv2fe : Fcl_float_var.FloatVar.t -> t
(** [fv2fe v] returns an expression which evaluates to [d] if
   the variable [v] is instantiated to [d]. *)
val fd2fe : Fcl_var.Fd.t -> t
(** [fd2fe x] returns an expression which evaluates to the smallest
   interval containing [v] if the integer variable [x] is instantiated
   to [v]. *)
val fe2fv : t -> Fcl_float_var.FloatVar.t
(** [fe2fv e] creates and returns a new variable [v] and posts
   the constraint [fv2fe v =.~ e] *)

(*** Access *)
val eval : t -> Fcl_float_domain.t
(** [eval e] returns the current evaluation of the expression [e]. *)
val get_vars : t -> (Fcl_float_var.FloatVar.t list * Fcl_var.Fd.t list)
(** [get_vars e] returns the couple [(l_floatVar,l_fd)] where
   [l_floatVar] (resp. [l_fd]) is the list of the interval variables
   (resp. integer variables) of the expression [e]. *)

(** {2 Construction of Arithmetic Expressions} *)

val (+.~) : t -> t -> t
val (-.~) : t -> t -> t
val ( *.~): t -> t -> t
val (/.~) : t -> t -> t
(** Addition, substraction, multiplication and division on expressions. *)
val sum_fe : t array -> t
val sum_fv : Fcl_float_var.FloatVar.t array -> t
(** [sum_fe exps] (resp. [sum_fv vars]) returns the sum of all
   the elements of an array of expressions (resp. variables). *)
val abs_fe : t -> t
(** Absolute value on expressions. *)
val pow_fe : t -> int -> t
(** Exponentiation of an expression to an integer value. *)
val neg_fe : t -> t
(** Negation of an expression. *)
val sqrt_fe : t -> t
(** Square root of an expression. *)
val sqr_fe : t -> t
(** Square of an expression. *)
val exp_fe : t -> t
val log_fe : t -> t
(** Exponential and natural logarithm of an expression. *)
val sin_fe : t -> t
val cos_fe : t -> t
val tan_fe : t -> t
val cot_fe : t -> t
(** The usual trigonometric functions on expressions. *)
val asin_fe : t -> t
val acos_fe : t -> t
val atan_fe : t -> t
val acot_fe : t -> t
(** The usual inverse trigonometric functions on expressions. *)

(** {2 Arithmetic Constraints on Expressions} *)

val (=.~) : t -> t -> Fcl_cstr.t
val (<=.~) : t -> t -> Fcl_cstr.t
val (>=.~) : t -> t -> Fcl_cstr.t
(** Equality and inequality constraints on expressions. *)

val bc3_eq : t -> t -> Fcl_cstr.t
val bc4_eq : t -> t -> Fcl_cstr.t
val bc4_le : t -> t -> Fcl_cstr.t


(** {2 Reified Inequalities on Expressions} *)
val (<=.~~) : t -> t -> t
val (>=.~~) : t -> t -> t
(** Reified inequalities on expressions. *)


val is_le_cstr : t -> t -> Fcl_var.Fd.t -> Fcl_cstr.t
(** [is_le_cstr e1 e2 b] returns a constraint [c] which ensures that
   the  boolean (0..1) variable [b] is instantiated to 1 iff [e1] is
   less or equal to [e2] and to 0 iff [e1] is strictly greater
   than [e2]. *)
val is_le : t -> t -> Fcl_var.Fd.t
(** [is_le e1 e2] creates and returns a boolean variable [b] and posts
   the constraint [is_le_cstr e1 e2 b]. *)


val bc3_eq : t -> t -> Fcl_cstr.t
(** undocumented *)
val bc4_eq : t -> t -> Fcl_cstr.t
(** undocumented *)
val bc4_le : t -> t -> Fcl_cstr.t
(**undocumented *)
val bc3_is_le_cstr : t -> t -> Fcl_var.Fd.t -> Fcl_cstr.t
val bc3_is_le : t -> t -> Fcl_var.Fd.t
val bc4_is_le_cstr : t -> t -> Fcl_var.Fd.t -> Fcl_cstr.t
val bc4_is_le : t -> t -> Fcl_var.Fd.t
