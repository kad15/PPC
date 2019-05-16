open Fcl_var
open Fcl_float_var
module A = Fcl_float_arith
module D = Fcl_float_domain
module S = Fcl_stak

let unit_false () = false

let min_val_of_var v = D.min_val (FloatVar.min v) 
let max_val_of_var v = D.max_val (FloatVar.max v)

(***************************************************************)
(* worse replacement on solve_with_return for compute_greatest *)
(*
let decr_max_incr_min xs i j =
  let c = compare (ub xs.(j)) (ub xs.(i)) in
  if c <> 0 then c else compare (lb xs.(i)) (lb xs.(j))

let upper_class xs idxs =
  let idxs = List.sort (decr_max_incr_min xs) idxs in
  let rec loop low up = function
      [] -> []
    | i :: is ->
	let up = min up (max_val_of_var xs.(i))
	and low = max low (min_val_of_var xs.(i)) in
	if up < low then [] else i :: loop low up is in
  let upper =
    match idxs with
      [] -> []
    | i :: is ->
          i :: loop (min_val_of_var xs.(i)) (max_val_of_var xs.(i)) is in
  Printf.printf "xs:\n%a\nupper:\n" FloatVar.fprint_array xs;
  List.iter (fun i -> Printf.printf "(%d)%a " i FloatVar.fprint xs.(i)) upper;
  Printf.printf "\n\n%!";
  upper
*)
(***************************************************************)

let classify x y =
  if max_val_of_var x < min_val_of_var y then 1
  else if max_val_of_var y < min_val_of_var x then -1
  else 0

let max2_cstr x y m =
  let name = "Fcl_float_array.max2" in
  let delay_ c =
    FloatVar.delay [FloatVar.on_refine] x c;
    FloatVar.delay [FloatVar.on_refine] y c;
    FloatVar.delay [FloatVar.on_refine] m c in

  let lower v maxm =
    match FloatVar.value v with
      Val v -> if D.strictly_inf maxm v then S.fail name
    | Unk _ -> FloatVar.refine_up v maxm in

  let update _ =
    Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - before update : %a = max[|%a; %a|]\n" name FloatVar.fprint m FloatVar.fprint x FloatVar.fprint y);
    let c = classify x y in
    if c > 0 then begin
      Fcl_cstr.post (A.(=.~) (A.fv2fe m) (A.fv2fe y));
      true end
    else if c < 0 then begin
      Fcl_cstr.post (A.(=.~) (A.fv2fe m) (A.fv2fe x));
      true end
    else begin
      let maxm = FloatVar.max m in
      lower x maxm;
      lower y maxm;
      let lbx = min_val_of_var x and lby = min_val_of_var y in
      let lb = max lbx lby in
      let ubx = max_val_of_var x and uby = max_val_of_var y in
      let ub = max ubx uby in
      begin match FloatVar.value m with
	Val m -> if D.min_val m > ub then Fcl_stak.fail name
      | Unk _ -> FloatVar.refine_low_up m (D.float2elt lb) (D.float2elt ub) end;
      Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - after update : %a = max[|%a; %a|]\n" name FloatVar.fprint m FloatVar.fprint x FloatVar.fprint y);
      FloatVar.is_bound x && FloatVar.is_bound y && FloatVar.is_bound m end in

  let fprint chout =
    Printf.fprintf chout "%s: %a = max(%a, %a)"
      name FloatVar.fprint m FloatVar.fprint x FloatVar.fprint y in

  Fcl_cstr.create ~name ~fprint update delay_

let max2 x y =
  let max_ = max (max_val_of_var x) (max_val_of_var y)
  and min_ = max (min_val_of_var x) (min_val_of_var y) in
  let m = Fcl_float_var.FloatVar.create (D.floatsI min_ max_) in
  Fcl_cstr.post (max2_cstr x y m);
  m


let min2_cstr x y m =
  let name = "Fcl_float_array.min2" in
  let delay_ c =
    FloatVar.delay [FloatVar.on_refine] x c;
    FloatVar.delay [FloatVar.on_refine] y c;
    FloatVar.delay [FloatVar.on_refine] m c in

  let upper v minm =
    match FloatVar.value v with
      Val v -> if D.strictly_inf v minm then S.fail name
    | Unk _ -> FloatVar.refine_low v minm in

  let update _ =
    Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - before update : %a = max[|%a; %a|]\n" name FloatVar.fprint m FloatVar.fprint x FloatVar.fprint y);
    let c = classify x y in
    if c > 0 then begin
      Fcl_cstr.post (A.(=.~) (A.fv2fe m) (A.fv2fe x));
      true end
    else if c < 0 then begin
      Fcl_cstr.post (A.(=.~) (A.fv2fe m) (A.fv2fe y));
      true end
    else begin
      let minm = FloatVar.min m in
      upper x minm;
      upper y minm;
      let lbx = min_val_of_var x and lby = min_val_of_var y in
      let lb = min lbx lby in
      let ubx = max_val_of_var x and uby = max_val_of_var y in
      let ub = min ubx uby in
      begin match FloatVar.value m with
	Val m -> if D.max_val m < lb then Fcl_stak.fail name
      | Unk _ -> FloatVar.refine_low_up m (D.float2elt lb) (D.float2elt ub) end;
      Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - after update : %a = max[|%a; %a|]\n" name FloatVar.fprint m FloatVar.fprint x FloatVar.fprint y);
      FloatVar.is_bound x && FloatVar.is_bound y && FloatVar.is_bound m end in

  Fcl_cstr.create ~name update delay_

let min2 x y =
  let max_ = min (max_val_of_var x) (max_val_of_var y)
  and min_ = min (min_val_of_var x) (min_val_of_var y) in
  let m = Fcl_float_var.FloatVar.create (D.floatsI min_ max_) in
  Fcl_cstr.post (min2_cstr x y m);
  m

(* builds [0;1;...;n-1] *)
let intlist n =
  let rec loop i l = 
    if i = 0 then l else
    let i1 = i-1 in loop i1 (i1 :: l) in
  loop n []

let get_extr_val f comp l =
  let rec iter m l =
    match l with
      [] -> m
    | x :: xs ->
	let mx = f x in 
	if comp m mx then iter mx xs else iter m xs in
  match l with
    [] -> Fcl_stak.fail "empty list"
  | x :: xs -> iter (f x) xs

let get_max_val t = get_extr_val (fun i -> max_val_of_var t.(i)) (<)
let get_min_val t = get_extr_val (fun i -> min_val_of_var t.(i)) (>)

let compute_greatest t l var =
  let rec loop great_list = function
      [] -> begin
	match great_list with
	  [] -> Fcl_stak.fail "Fcl_float_array.max: no solution"
	| _ -> great_list end
    | x :: xs ->
	let (great, l1) =
	  List.fold_left 
	    (fun (g, l) i ->
	      let cix = classify t.(i) t.(x) in
	      if cix = 1 then (g, l)
	      else if cix = -1 then (false, i :: l)
	      else (g, i :: l))
	    (true, []) xs  in
	if great && max_val_of_var t.(x) >= min_val_of_var var then
	  loop (x :: great_list) l1
	else loop great_list l1 in
  loop [] l

(* max with dynamic maintaining of the greatests list *)
let max_cstr_imp tab var =
  let name = "Fcl_float_array.max" in
  let n = Array.length tab in
  let t = Array.copy tab in
  let greatests = Fcl_stak.ref (intlist n) in

  let delay_ c =
    Array.iter (fun x -> FloatVar.delay [FloatVar.on_refine] x c) t;
    FloatVar.delay [FloatVar.on_refine] var c in

  let update _ =
    Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - before update : %a = max%a\n" name FloatVar.fprint var FloatVar.fprint_array tab);
    let g = compute_greatest tab (Fcl_stak.get greatests) var in
    S.set greatests g;
    let max_greatests = get_max_val t g
    and min_greatests = get_min_val t g in
    let maxi_var = max_val_of_var var in
    if maxi_var > max_greatests then begin
      match FloatVar.value var with
	Val v ->
	  let inter_ = 
	    D.intersect v (D.floatsI min_greatests max_greatests) in
	  if D.is_empty inter_ then S.fail name
      | Unk a ->
	  let da = FloatAttr.dom a in
	  let inter_ = 
	    D.intersect da (D.floatsI min_greatests max_greatests) in
	  FloatVar.refine var inter_ end
    else begin
      let new_max = FloatVar.max var in
      List.iter
	(fun x ->
	  match FloatVar.value t.(x) with
	    Val v ->
	      if D.min_val v > maxi_var then S.fail (name ^ ": is_empty")
	  | Unk a ->
	      let da = FloatAttr.dom a in
	      let remove_h = D.remove_up new_max da in
	      FloatVar.refine t.(x) remove_h)
	(S.get greatests) end;

    if min_val_of_var var < min_greatests then begin
      match FloatVar.value var with 
	Val v -> 
	  let inter_ = 
	    D.intersect v (D.floatsI min_greatests max_greatests) in
	  if D.is_empty inter_ then S.fail name
      | Unk a ->
	  let da = FloatAttr.dom a in
	  let inter_ =
	    D.intersect da (D.floatsI min_greatests max_greatests) in
	  FloatVar.refine var inter_ end;
    
    Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - after update : %a = max%a\n" name FloatVar.fprint var FloatVar.fprint_array tab);
    
    match g with
      [] -> S.fail (name ^ ": empty list")
    | [x] ->
	Fcl_cstr.post (A.(=.~) (A.fv2fe t.(x)) (A.fv2fe var));
	true
    | _ -> false in

  Fcl_cstr.create ~name update delay_

let max_cstr tab var =
  let n = Array.length tab in
  if n = 0 then Fcl_cstr.zero
  else if n = 1 then A.(=.~) (A.fv2fe tab.(0)) (A.fv2fe var)
  else if n = 2 then max2_cstr tab.(0) tab.(1) var
  else max_cstr_imp tab var  

let max tab =
  let n = Array.length tab in
  if n = 0 then
    Fcl_stak.fail "Fcl_float_array.max: empty array"
  else if n = 1 then tab.(0)
  else if n = 2 then max2 tab.(0) tab.(1)
  else
    let (_, max_) = 
      Fcl_misc.arg_max_array (fun x -> max_val_of_var x) tab
    and (_, min_) =
      Fcl_misc.arg_max_array (fun x -> min_val_of_var x) tab in
    let m = Fcl_float_var.FloatVar.create (D.floatsI min_ max_) in
    Fcl_cstr.post (max_cstr_imp tab m);
    m



let compute_lowest t l var =
  let rec loop low_list = function
      [] -> begin
	match low_list with
	  [] -> Fcl_stak.fail "Fcl_float_array.min: no solution"
	| _ -> low_list end
    | x :: xs ->
	let (low, l1) =
	  List.fold_left 
	    (fun (g, l) i ->
	      let cix = classify t.(x) t.(i) in
	      if cix = 1 then (g, l)
	      else if cix = -1 then (false, i :: l)
	      else (g, i :: l))
	    (true, []) xs  in
	if low && min_val_of_var t.(x) <= max_val_of_var var then
	  loop (x :: low_list) l1
	else loop low_list l1 in
  loop [] l

(* min with dynamic maintaining of the greatests list *)
let min_cstr_imp tab var =
  let name = "Fcl_float_array.min" in
  let n = Array.length tab in
  let t = Array.copy tab in
  let lowests = Fcl_stak.ref (intlist n) in

  let delay_ c =
    Array.iter (fun x -> FloatVar.delay [FloatVar.on_refine] x c) t;
    FloatVar.delay [FloatVar.on_refine] var c in

  let update _ =
    Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - before update : %a = max%a\n" name FloatVar.fprint var FloatVar.fprint_array tab);
    let g = compute_lowest tab (Fcl_stak.get lowests) var in
    S.set lowests g;
    let max_lowests = get_max_val t g
    and min_lowests = get_min_val t g in
    let mini_var = min_val_of_var var in
    if mini_var < min_lowests then begin
      match FloatVar.value var with
	Val v ->
	  let inter_ = 
	    D.intersect v (D.floatsI min_lowests max_lowests) in
	  if D.is_empty inter_ then S.fail name
      | Unk a ->
	  let da = FloatAttr.dom a in
	  let inter_ = 
	    D.intersect da (D.floatsI min_lowests max_lowests) in
	  FloatVar.refine var inter_ end
    else begin
      let new_min = FloatVar.min var in
      List.iter
	(fun x ->
	  match FloatVar.value t.(x) with
	    Val v ->
	      if D.max_val v < mini_var then
		S.fail (name ^ ": is_empty")
	  | Unk a ->
	      let da = FloatAttr.dom a in
	      let remove_l = D.remove_low new_min da in
	      FloatVar.refine t.(x) remove_l)
	(S.get lowests) end;

    if max_val_of_var var > max_lowests then begin
      match FloatVar.value var with 
	Val v -> 
	  let inter_ = 
	    D.intersect v (D.floatsI min_lowests max_lowests) in
	  if D.is_empty inter_ then S.fail name
      | Unk a ->
	  let da = FloatAttr.dom a in
	  let inter_ =
	    D.intersect da (D.floatsI min_lowests max_lowests) in
	  FloatVar.refine var inter_ end;
    
    Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - after update : %a = max%a\n" name FloatVar.fprint var FloatVar.fprint_array tab);
    
    match g with
      [] -> S.fail (name ^ ": empty list")
    | [x] ->
	Fcl_cstr.post (A.(=.~) (A.fv2fe t.(x)) (A.fv2fe var));
	true
    | _ -> false in

  Fcl_cstr.create ~name update delay_

let min_cstr tab var =
  let n = Array.length tab in
  if n = 0 then Fcl_cstr.zero
  else if n = 1 then A.(=.~) (A.fv2fe tab.(0)) (A.fv2fe var)
  else if n = 2 then min2_cstr tab.(0) tab.(1) var
  else min_cstr_imp tab var

let min tab =
  let n = Array.length tab in
  if n = 0 then
    Fcl_stak.fail "Fcl_float_array.min: empty array"
  else if n = 1 then tab.(0)
  else if n = 2 then min2 tab.(0) tab.(1)
  else
    let (_, min_) =
      Fcl_misc.arg_min_array
	(fun x -> min_val_of_var x) tab
    and (_, max_) =
      Fcl_misc.arg_min_array
	(fun x -> max_val_of_var x) tab in
    let m = Fcl_float_var.FloatVar.create (D.floatsI min_ max_) in
    Fcl_cstr.post (min_cstr_imp tab m);
    m


(***

let new_max_array xs x =
  let n = Array.length xs in
  let name = "Fcl_float_array.max" in
  let delay c =
    Array.iter
      (fun x -> FloatVar.delay [FloatVar.on_refine] x c) xs;
    FloatVar.delay [FloatVar.on_refine] x c
  and update _ =
    (* Try to decide which one is the greatest *)
    let greatest = ref 0 and max_greatest = ref (max_val_of_var xs.(0)) in
    for i = 1 to n - 1 do
      let max_i = max_val_of_var xs.(i) in
      if max_i > !max_greatest then begin
	greatest := i; max_greatest := max_i
      end
    done;
    let min_greatest = min_val_of_var xs.(!greatest) in
    try
      for i = 0 to n - 1 do
	if i <> !greatest && max_val_of_var xs.(i) > min_greatest then
	  raise Exit
      done; (* We have found the greatest element *)
      Fcl_cstr.post (A.(=.~) (A.fv2fe x) (A.fv2fe xs.(!greatest)));
      true
    with
      Exit ->
	(* All the xs are smaller than the max of max *)
      	let maxx = max_val_of_var x in
      	let x_leq_max xi =
	  match FloatVar.value xi with
	    Val v ->
	      if D.min_val v > maxx then Fcl_stak.fail name
	  | Unk a ->
	      let remove_h = D.remove_up (D.floatI maxx) (FloatAttr.dom a) in
	      FloatVar.refine xi remove_h in
	Array.iter x_leq_max xs;
	(* greatest min of xs <= x <= greatest max of xs *)
      	let greatest_min =
	  Array.fold_left
	    (fun r xi -> Pervasives.max (min_val_of_var xi) r)
	    (-. max_float) xs in
	begin
	  match FloatVar.value x with
	    Val x -> assert (D.max_val x >= greatest_min)
      	  | Unk a ->
	      let d =
		D.intersect
		  (D.floatsI greatest_min (max_val_of_var xs.(!greatest)))
		  (FloatAttr.dom a) in
	      FloatVar.refine x d
	end;
	false  in

  Fcl_cstr.create ~name update delay
    
let max_cstr xs x =
  let n = Array.length xs in
  if n = 0 then
    invalid_arg "Fcl_float_array.max_cstr: empty array"
  else if n = 1 then A.(=.~) (A.fv2fe xs.(0)) (A.fv2fe x) else
  (* To prevent array modifications by the user *)
  let xs = Array.copy xs in
  new_max_array xs x

let max xs =
  if Array.length xs = 1 then xs.(0) else
  let (_, max_) = 
    Fcl_misc.arg_max_array (fun x -> max_val_of_var x) xs
  and (_, min_) =
    Fcl_misc.arg_min_array (fun x -> min_val_of_var x) xs in
  let x = Fcl_float_var.FloatVar.create (D.floatsI min_ max_) in
  Fcl_cstr.post (max_cstr xs x);
  x
***)
