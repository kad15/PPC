module A=Array
open Fcl_goals
open Fcl_var
open Fcl_float_var
module D = Fcl_float_domain
module C = Fcl_cstr
module FI = Fcl_float_interval

type order = Incr | Decr
let instantiate ?(order = Decr) var =
  create_rec ~name:"Goals.Float.instantiate"
    (fun self ->
      match FloatVar.value var with
	Val _ -> success
      | Unk d ->
	  let middle = D.mid (FloatAttr.dom d) in
	  let i = FloatAttr.id d in
	  let upper =
	    atomic
	      (fun () ->
		Fcl_debug.call 'f' (fun f -> Printf.fprintf f "ref_up: %d <= %f\n" i (FI.max_val middle));
		FloatVar.refine_low var middle)
	  and lower =
	    atomic
	      (fun () ->
		Fcl_debug.call 'f' (fun f -> Printf.fprintf f "ref_low: %d > %f\n" i (FI.max_val middle));
		FloatVar.refine_up var middle) in
	  match order with
	    Decr -> (upper ||~ lower) &&~ self
	  | Incr -> (lower ||~ upper) &&~ self)


let min_val_of_var v = D.min_val (FloatVar.min v)
let max_val_of_var v = D.max_val (FloatVar.max v)

let minimize_continue ?(step=0.) goal cost compute_solution =
  let improve c = c -. step *. abs_float c in
  let best_cost = ref (max_val_of_var cost) in

  let rec bt_until l =
    (* Backtrack until lower bound better than current cost, staying above [l] *)
    let gs = Fcl_stak.backtrack () in
    if min_val_of_var cost < !best_cost then ignore (Fcl_stak.save gs)
    else if Fcl_stak.older (Fcl_stak.level ()) l then Fcl_stak.fail "float_continue"
    else bt_until l in

  let restore_max =
    let name = "Fcl_float_goals.Goals.restore_max" in
    let update _ =
      (*Printf.printf "cost=%a best_cost=%f\n" FloatVar.fprint cost !best_cost; flush stdout;*)
      match FloatVar.value cost with
	Val v ->
	  if D.max_val v > !best_cost then
	    Fcl_stak.fail name
	  else true
      | Unk attr -> 
	  FloatVar.refine_up cost (D.floatI !best_cost);
	  false
    and delay x = C.delay [Fcl_goals.on_choice_point] x in
    C.create ~name update delay in

  let found_one l =
    atomic ~name:"Fcl_float_goals.found_one"
      (fun () ->
	let c = max_val_of_var cost in
	compute_solution c;
	best_cost := improve c;
	bt_until l;
	Fcl_stak.fail "Fcl_float_goals.minimize_more")  in

  let init =
    create ~name:"Fcl_float_goals.continue_init"
      (fun () -> C.post restore_max; goal)
      () in

  create 
    (fun () ->
      let l = Fcl_stak.level () in
      init &&~ found_one l)
    ()

