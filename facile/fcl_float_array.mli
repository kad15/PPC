(** {1 Constraints over Arrays of Interval Variables} *)

val max : Fcl_float_var.FloatVar.t array -> Fcl_float_var.FloatVar.t
(** [max vars] returns a variable constrained to be equal
   to the variable that will be instantiated to the maximal
   value among all the variables in the array [vars]. Not reifiable. *)
val max_cstr : Fcl_float_var.FloatVar.t array -> Fcl_float_var.FloatVar.t -> Fcl_cstr.t
(** [max_cstr vars maxi] returns a constraint equivalent to
  [floatVar2e (max vars) =~ fd2e maxi]. Not reifiable. *) 

val min : Fcl_float_var.FloatVar.t array -> Fcl_float_var.FloatVar.t
(** [min vars] returns a variable constrained to be equal
   to the variable that will be instantiated to the minimal
   value among all the variables in the array [vars]. Not reifiable. *)
val min_cstr : Fcl_float_var.FloatVar.t array -> Fcl_float_var.FloatVar.t -> Fcl_cstr.t
(** [min_cstr vars mini] returns a constraint equivalent to
  [floatVar2e (min vars) =~ fd2e mini]. Not reifiable. *) 
