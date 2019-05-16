module I = Interval
open I
type t =I.interval

let everything = I.everything
let p_i = sup everything
let m_i = inf everything
let empty = I.empty
let piI = I.piI

let is_empty = I.is_empty
let is_point = I.is_point
let size d = sup d -. inf d

let compare =I.compare

let fprint= I.fprint

let min d = I.floatI (inf d)
let max d = I.floatI (sup d)
let mid d = I.floatI (I.mid d)
let mid_val = I.mid
let min_val = inf
let max_val = sup

let included e d = sup e <= sup d && inf e >= inf d
let union = I.hullIII
let intersect = I.intersectIII

let remove_low e d =  
  let e_low = inf e and d_high = sup d in
  if d_high < e_low then empty
  else if inf d < e_low then floatsI e_low d_high
  else d

let remove_up e d =
  let e_high = sup e and d_low = inf d in
  if e_high < d_low then empty
  else if sup d > e_high then floatsI d_low e_high
  else d

let strictly_inf a b = sup a < inf b

let addIII = I.addIII
let subIII = I.subIII
let mulIII = I.mulIII
let divIII = I.divIII
let powIII = I.powIII
let absII = I.absII
let powII = I.powII
let negII = I.negII
let sqrtII = I.sqrtII
let sqrII = I.sqrII
let expII = I.expII
let logII = I.logII
let intiI = I.intI
let intsI = I.intsI
let floatI = I.floatI
let floatsI = I.floatsI

let acosII = I.acosII
let acoshII = I.acoshII
let acotII = I.acotII
let acothII = I.acothII
let asinII = I.asinII
let asinhII = I.asinhII
let atanII = I.atanII
let atanhII = I.atanhII
let cosII = I.cosII
let coshII = I.coshII
let cotII = I.cotII
let cothII = I.cothII
let sinII = I.sinII
let sinhII = I.sinhII
let tanII = I.tanII
let tanhII = I.tanhII
