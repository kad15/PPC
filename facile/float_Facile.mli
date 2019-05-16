module FloatInterval: sig 
  type t 
  val everything : t 
  val p_i : float
  val m_i : float
  val empty : t
  val piI : t
  val is_empty : t -> bool
  val is_point : t -> bool
  val size : t -> float
      
  val compare : t -> t -> int
      
  val fprint : out_channel -> t -> unit 
      
  val included : t -> t -> bool
      
  val union : t -> t -> t
  val intersect : t -> t -> t
      
  val strictly_inf : t -> t -> bool
      
  val min : t -> t
  val max : t -> t
  val mid : t -> t
  val mid_val : t -> float
  val min_val : t -> float
  val max_val : t -> float
      
  val remove_low : t -> t -> t
  val remove_up : t -> t -> t
      
  val addIII : t -> t -> t
  val subIII : t -> t -> t
  val mulIII : t -> t -> t
  val divIII : t -> t -> t
      
  val absII : t -> t
  val powIII : t -> t -> t
  val powII : t -> int -> t
  val negII :t -> t
  val sqrtII :t -> t
  val sqrII : t -> t
  val expII : t -> t
  val logII : t -> t
  val intiI : int -> t
  val intsI : int -> int -> t
  val floatI : float -> t
  val floatsI : float -> float -> t
      
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
end


module FloatDomain : sig
  type elt = FloatInterval.t
  type t
  type size = float
  val positive : t
  val negative : t
  val everything :t
  val piI : t
  val empty : t
  val is_empty : t -> bool
  val get_precision : unit -> float
  val set_precision : float -> unit
  val get_delta_ratio : unit -> float
  val set_delta_ratio : float -> unit
  val is_bound : t -> bool
  val compare_elt : elt -> elt -> int
  val compare : t -> t -> int
  val fprint_elt : out_channel -> elt -> unit
  val fprint : out_channel -> t -> unit
  val min : t -> elt
  val max : t -> elt
  val mid : t -> elt
  val mid_val : t -> float
  val min_val : t -> float
  val max_val : t -> float
  val int_bounds : t -> int * int
      
  val mem : elt -> t -> bool
  val interval : elt -> elt -> t
  val remove_low : elt -> t -> t
  val remove_up : elt -> t -> t
  val included : t -> t -> bool
  val min_max : t -> elt * elt
  val size : t -> size
  val strictly_inf : elt -> elt -> bool
  val singleton : elt -> t    
      
  val union : t -> t -> t
  val intersect : t -> t -> t
      
  val intiI : int -> t
  val intsI : int -> int -> t
  val floatI : float -> t
  val floatsI : float -> float -> t
  val float2elt : float -> elt

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
end

module FloatAttr : Facile.Var.ATTR
 with 
     type domain = FloatDomain.t
     and type elt = FloatDomain.elt
     and type size = float

module FloatVar : Facile.Var.BASICVAR
 with
     type domain = FloatDomain.t
     and type elt = FloatDomain.elt
     and type size = float
     and type attr = FloatAttr.t
     and type event = FloatAttr.event
      
module FloatGoals : sig
  type order = Incr | Decr
  val instantiate : ?order:order -> FloatVar.t -> Facile.Goals.t
  val minimize_continue : ?step:float -> Facile.Goals.t -> FloatVar.t -> (float -> unit) -> Facile.Goals.t
end


module FloatCstr : sig
  type t
  val hc4_equal : t -> Facile.Cstr.t
  val hc4_le : t -> Facile.Cstr.t
  val equal : t -> t -> Facile.Cstr.t
  val le : t -> t -> Facile.Cstr.t
  val bc3_equal : t -> Facile.Cstr.t
  val bc4_equal : t -> Facile.Cstr.t
  val bc4_le : t -> Facile.Cstr.t
  val is_le_cstr : t -> t -> Facile.Var.Fd.t -> Facile.Cstr.t
  val is_le : t -> t -> Facile.Var.Fd.t
end

module FloatArith : sig 
  type t
  val f2fe : float -> t
  val dom2fe : FloatDomain.t -> t
  val fv2fe : FloatVar.t -> t
  val fd2fe : Facile.Var.Fd.t -> t
  val fe2fv : t -> FloatVar.t
  val ( +.~ ) : t -> t -> t
  val ( -.~ ) : t -> t -> t
  val ( *.~ ) : t -> t -> t
  val ( /.~ ) : t -> t -> t
  val sum_fe : t array -> t
  val sum_fv : FloatVar.t array -> t
  val abs_fe : t -> t
  val pow_fe : t -> int -> t
  val neg_fe : t -> t
  val sqrt_fe : t -> t
  val sqr_fe : t -> t
  val exp_fe : t -> t
  val log_fe : t -> t
  val sin_fe : t -> t
  val cos_fe : t -> t
  val tan_fe : t -> t
  val cot_fe : t -> t
  val asin_fe : t -> t
  val acos_fe : t -> t
  val atan_fe : t -> t
  val acot_fe : t -> t
  val ( =.~ ) : t -> t -> Facile.Cstr.t
  val ( <=.~ ) : t -> t -> Facile.Cstr.t
  val ( >=.~ ) : t -> t -> Facile.Cstr.t
  val bc3_eq : t -> t -> Facile.Cstr.t
  val bc3_is_le_cstr : t -> t -> Facile.Var.Fd.t -> Facile.Cstr.t
  val bc3_is_le : t -> t -> Facile.Var.Fd.t
  val bc4_eq : t -> t -> Facile.Cstr.t
  val bc4_le : t -> t -> Facile.Cstr.t
  val bc4_is_le_cstr : t -> t -> Facile.Var.Fd.t -> Facile.Cstr.t
  val bc4_is_le : t -> t -> Facile.Var.Fd.t
  val is_le_cstr : t -> t -> Facile.Var.Fd.t -> Facile.Cstr.t
  val is_le  : t -> t -> Facile.Var.Fd.t
  val ( <=.~~ ) : t -> t -> t
  val ( >=.~~ ) : t -> t -> t
  val eval : t -> FloatDomain.t
  val get_vars : t -> (FloatVar.t list * Facile.Var.Fd.t list)
end

module FloatArray : sig
  val max_cstr :FloatVar.t array -> FloatVar.t -> Facile.Cstr.t
  val max : FloatVar.t array -> FloatVar.t
  val min_cstr :FloatVar.t array -> FloatVar.t -> Facile.Cstr.t
  val min : FloatVar.t array -> FloatVar.t
end
