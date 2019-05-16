type interval

external entire : unit -> interval = "entire_" 
external empty : unit -> interval = "empty_"
external pi : unit -> interval = "pi_"
external neg_infty : unit -> interval = "neg_infty_"
external pos_infty :unit -> interval = "pos_infty_"
external zero : unit -> interval = "zero_"
external one : unit -> interval = "one_"

let everything = entire ()
let empty = empty ()
let piI = pi ()
let neg_inftyI = neg_infty ()
let pos_inftyI = pos_infty ()
let zeroI = zero ()
let oneI = one ()

external create : float -> float -> interval = "create_"

(* arithmetic operators *)
external addIII : interval -> interval -> interval = "addIII_"
external subIII : interval -> interval -> interval = "subIII_"
external mulIII : interval -> interval -> interval = "mulIII_"
external divIII : interval -> interval -> interval = "divIII_"
external negII : interval -> interval = "negII_"

(* access and information *)
external is_empty : interval -> bool = "is_empty_"
external is_point : interval -> bool = "is_point_"
external mid : interval -> float = "mid_"
external inf : interval -> float = "inf_"
external sup : interval -> float = "sup_"
external inf_sup : interval -> float * float = "inf_sup_"
external absII : interval -> interval = "absII_"
external diam : interval -> bool = "diam_"
external rel_diam : interval -> bool = "relDiam_"
external rad : interval -> bool = "rad_"
external mig : interval -> bool = "mig_"
external mag : interval -> bool = "mag_"
external dist : interval -> bool = "dist_"
external is_in : float -> interval -> bool = "in_"

(* set theoretic functions *)
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

(* interval relational functions *)

external seq : interval -> interval -> bool = "seq_"
external sne : interval -> interval -> bool = "sne_"
external sge : interval -> interval -> bool = "sge_"
external sgt : interval -> interval -> bool = "sgt_"
external sle : interval -> interval -> bool = "sle_"
external slt : interval -> interval -> bool = "slt_"

external ceq : interval -> interval -> bool = "ceq_"
external cne : interval -> interval -> bool = "cne_"
external cge : interval -> interval -> bool = "cge_"
external cgt : interval -> interval -> bool = "cgt_"
external cle : interval -> interval -> bool = "cle_"
external clt : interval -> interval -> bool = "clt_"

external peq : interval -> interval -> bool = "peq_"
external pne : interval -> interval -> bool = "pne_"
external pge : interval -> interval -> bool = "pge_"
external pgt : interval -> interval -> bool = "pgt_"
external ple : interval -> interval -> bool = "ple_"
external plt : interval -> interval -> bool = "plt_"

(* elementary functions *)
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


let intsI x y = create (float_of_int x) (float_of_int y)
let intI v =
  let float_v = float_of_int v in
  create float_v float_v
let floatI v = create v v

external floatsI : float -> float -> interval = "create_"

let compare x y = 
  let c_inf = compare (inf x) (inf y) in
  if c_inf = 0 then compare (sup x) (sup y) else c_inf

let sprint x = Printf.sprintf "[%.16f,%.16f]" (inf x) (sup x)

let fprint c x = Printf.fprintf c "[%.16f,%.16f]" (inf x) (sup x)
