(** {1 Goals Over Interval Variables} *)

(** {2 Instantiation} *)

type order = Incr | Decr
val instantiate : ?order:order -> Fcl_float_var.FloatVar.t -> Fcl_goals.t
(* [instantiate (?order:Decr) v] returns a goal that non-deterministically
   instantiates variable [v] by bissecting its domain with increasing or
   decreasing parts first according to the optional argument [order].
   [order] default value is [Decr]. *)

(** {2 Optimization} *)

val minimize_continue : ?step:float -> Fcl_goals.t ->  Fcl_float_var.FloatVar.t -> (float -> unit) -> Fcl_goals.t
(** [minimize ~step:0.0 goal cost solution] runs a
   Branch and Bound algorithm on [goal] for bound [cost], with an improvement
   of at least [step] times the best cost so far (in absolute value)
   between each solution found. Each time a solution is found, the [solution]
   function is called with the instantiation value of [cost]
   (which {\b must be instantiated} by [goal]) as argument; this
   function can therefore be used to store (e.g. in a reference) the current
   solution. Default [step] is [0.0]. [minimize] {b always fails}. *)

