(* $Id: fcl_float_var.mli,v 1.4 2006-01-30 15:47:55 allignol Exp $ *)

module FloatAttr : Fcl_var.ATTR
 with type domain = Fcl_float_domain.t
    and type elt = Fcl_float_domain.elt
    and type size = float

module FloatVar : Fcl_var.BASICVAR
 with
     type domain = Fcl_float_domain.t
     and type elt = Fcl_float_domain.elt
     and type size = float
     and type attr = FloatAttr.t
     and type event = FloatAttr.event

