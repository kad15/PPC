open Facile
open Easy

(* splitting a domain in two (once) *)
let () = let x = Fd.interval 0 12 in
let split = fun x ->
  if Fd.is_bound x then Goals.success else
  let (xmin,xmax) = Fd.min_max x in
  let mid =(xmin+xmax)/2 in
  Goals.atomic (fun () -> Fd.refine_up x mid) ||~
  Goals.atomic (fun () -> Fd.refine_low x (mid+1))
let gsplit = Goals.create split x
                                            
