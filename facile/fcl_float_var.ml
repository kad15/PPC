(* $Id: fcl_float_var.ml,v 1.2 2005-12-15 13:40:56 barnier Exp $ *)


module C = Fcl_cstr

module FloatAttr = Fcl_var.MakeAttr(Fcl_float_domain)
module FloatVar = Fcl_var.MakeVar(FloatAttr)
