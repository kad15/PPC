(* precision under which the domain is condidered to be bound *)
let epsilon = ref 1e-8
let get_precision () = !epsilon
let set_precision p = epsilon := p

(* minimum delta ratio to trigger propagation *)
let dr = ref 1e-2
let get_delta_ratio () = !dr
let set_delta_ratio ratio = dr:= ratio

module S = Fcl_float_interval

type elt = S.t
type t = S.t
type size = float

let positive = S.floatsI 0. S.p_i 
let negative = S.floatsI S.m_i 0.
let everything = S.everything
let piI = S.piI
let empty = S.empty
let is_empty  = S.is_empty 
let is_bound d = S.size d < !epsilon
let size  = S.size 

let compare_elt=compare 
let compare = S.compare

let fprint = S.fprint 
    
let fprint_elt = fprint

let min = S.min
let max = S.max
let mid = S.mid 
let mid_val = S.mid_val
let min_val = S.min_val
let max_val = S.max_val

let int_bounds dom =	
  let miniv =
    let (fm, im) = modf (min_val dom) in
    if  im < 0. || fm = 0. then truncate im else truncate im + 1
  and maxiv =
    let (fmax, imax) = modf (max_val dom) in
    if  imax > 0. || fmax = 0. then truncate imax else truncate imax - 1 in
  (miniv, maxiv)

let elt_value d = assert( is_bound d); d

let delta_width v =
  get_delta_ratio () *. size v +. get_precision ()
let affine_delta old current =
  let borne = delta_width current in
  min_val current -. min_val old > borne ||
  max_val old -. max_val current > borne
let constant_delta old current =
  let precision = get_precision () in
  min_val current -. min_val old > precision ||
  max_val old -. max_val current > precision

let dom_changed dom new_dom = 
  let borne = delta_width dom in
  min_val new_dom -. min_val dom > borne ||
  max_val dom -. max_val new_dom > borne 
    
let min_changed dom new_dom = 
  let borne = delta_width dom in
  min_val new_dom -. min_val dom > borne 
    
let max_changed dom new_dom =
  let borne = delta_width dom in
  max_val dom -. max_val new_dom > borne 

    
let mem = S.included
let interval = S.union

let remove_low = S.remove_low 

let remove_up = S.remove_up

let included = mem 
let min_max d = (min d, max d)
let strictly_inf = S.strictly_inf

let intiI = S.intiI
let intsI = S.intsI
let floatI = S.floatI
let floatsI = S.floatsI

let float2elt = S.floatI

let union = S.union
let intersect = S.intersect

let addIII = S.addIII
let subIII = S.subIII
let mulIII = S.mulIII
let divIII = S.divIII
let powIII = S.powIII
let powII = S.powII
let absII = S.absII
let negII = S.negII
let sqrtII = S.sqrtII
let sqrII = S.sqrII
let singleton x = x
let expII = S.expII
let logII = S.logII

let acosII = S.acosII
let acoshII = S.acoshII
let acotII = S.acotII
let acothII = S.acothII
let asinII = S.asinII
let asinhII = S.asinhII
let atanII = S.atanII
let atanhII = S.atanhII
let cosII = S.cosII
let coshII = S.coshII
let cotII = S.cotII
let cothII = S.cothII
let sinII = S.sinII
let sinhII = S.sinhII
let tanII = S.tanII
let tanhII = S.tanhII
