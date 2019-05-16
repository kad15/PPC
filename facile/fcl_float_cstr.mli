type bin_op = Mult | Div | Plus | Minus

type un_op =
    Abs | Pow of int | Neg | Sqrt | Sqr | Exp | Log
  | Sin | Cos | Tan | Cot | Asin | Acos | Atan | Acot

type t =
    Bin of bin_op * t * t
  | Un of un_op * t
  | Var of Fcl_float_var.FloatVar.t
  | Var_int of Fcl_var.Fd.t
  | Cst of Fcl_float_domain.elt
(* Type of arithmetic expression. *)

val eval_exp : t -> Fcl_float_domain.t
val get_var_list : t -> (Fcl_float_var.FloatVar.t list * Fcl_var.Fd.t list)
(* Returns the list of finite domain variables and the list of
   interval variables of an expression. *)
val simplify : t -> t

val minus : t -> t -> t

val hc4_equal : t -> Fcl_cstr.t
val hc4_le : t -> Fcl_cstr.t

val bc3_equal : t -> Fcl_cstr.t
val bc3_is_le_cstr : t -> Fcl_var.Fd.t -> Fcl_cstr.t
val bc3_is_le : t -> Fcl_var.Fd.t
val bc4_equal : t -> Fcl_cstr.t
val bc4_le : t -> Fcl_cstr.t
val bc4_is_le_cstr : t -> Fcl_var.Fd.t -> Fcl_cstr.t
val bc4_is_le : t -> Fcl_var.Fd.t

val equal : t -> t -> Fcl_cstr.t
val le : t -> t -> Fcl_cstr.t
(* Default constraints: currently [bc4_equal] and [bc4_le]. *)

val is_le_cstr : t -> t -> Fcl_var.Fd.t -> Fcl_cstr.t
val is_le : t -> t -> Fcl_var.Fd.t
(* Default constraints: currently [hc4_is_le] and [hc4_is_le_cstr]. *)
