open Fcl_var
open Fcl_float_var
open Fcl_float_domain
module C = Fcl_cstr
module FI = Fcl_float_interval

type bin_op = Mult | Div | Plus | Minus

let bin2str = function
    Mult -> "*."
  | Div -> "/."
  | Plus -> "+."
  | Minus -> "-."

type un_op =
    Abs | Pow of int | Neg
  | Sqrt | Sqr | Exp | Log
  | Sin | Cos | Tan | Cot
  | Asin | Acos | Atan | Acot

let un2str = function
    Abs -> "abs" | Pow n -> Printf.sprintf "pow%d" n | Neg -> "neg"
  | Sqrt -> "sqrt" | Sqr -> "sqr" | Exp -> "exp" | Log -> "log"
  | Sin | Cos | Tan | Cot
  | Asin | Acos | Atan | Acot -> "trigo"

type t =
    Bin of bin_op * t * t
  | Un of un_op * t
  | Var of FloatVar.t
  | Var_int of Fd.t
  | Cst of Fcl_float_domain.elt

let rec fprint_exp c = function
    Bin (op, t1, t2) ->
      Printf.fprintf c "%s(%a,%a)" (bin2str op) fprint_exp t1 fprint_exp t2
  | Un (op, t) -> Printf.fprintf c "%s(%a)" (un2str op) fprint_exp t
  | Var v -> Fcl_float_var.FloatVar.fprint c v
  | Var_int vi -> Fcl_var.Fd.fprint c vi
  | Cst elt -> Fcl_float_domain.fprint_elt c elt

let rec size = function
    Bin (_, t1, t2) -> 1 + size t1 + size t2
  | Un (op, t) -> 1 + size t
  | _ -> 1


type val_expr = 
    Binv of bin_op * val_expr * val_expr * Fcl_float_domain.t
  | Unv of un_op * val_expr * Fcl_float_domain.t
  | Varv of FloatVar.t  * Fcl_float_domain.t
  | Var_intv of Fd.t * Fcl_float_domain.t
  | Cstv of Fcl_float_domain.t

type classified_var = Int of Fd.t | Float of FloatVar.t

let zero = floatI 0.
let one = floatI 1. 
let two = floatI 2.

let zero_elt = float2elt 0.

let half_pi = divIII piI (intiI 2) 
let two_pi = mulIII piI (intiI 2)

let k_2pi vx =
  let k = divIII vx two_pi in
  let k_min = 
    if min_val vx < 0. then truncate (min_val k) -1
    else truncate (min_val k) in
  let k_max =
    if max_val vx < 0. then truncate (max_val k) -1
    else truncate (max_val k) in
  mulIII (floatsI(float k_min)(float k_max)) two_pi 

let pow i x = powII x i

let eval_un_op = function
    Abs -> absII
  | Pow i -> pow i
  | Neg -> negII
  | Sqrt -> sqrtII
  | Sqr -> sqrII
  | Exp -> expII
  | Log -> logII
  | Sin -> sinII
  | Cos -> cosII 
  | Tan -> tanII
  | Cot -> cotII
  | Asin -> asinII
  | Acos -> acosII
  | Atan -> atanII
  | Acot -> acotII

let eval_bin_op = function
    Mult -> mulIII
  | Div  -> divIII
  | Minus-> subIII
  | Plus -> addIII

let eval varf var_intf e =
  let rec loop = function
      Cst elt -> elt
    | Bin (bin_op, g, d) -> eval_bin_op bin_op (loop g) (loop d)
    | Un (un_op, e) -> eval_un_op un_op (loop e)
    | Var v -> begin
	match FloatVar.value v with
	  Val vv -> vv
	| Unk attr -> varf attr end
    | Var_int v -> begin
	match Fd.value v with
	  Val vv -> intiI vv
	| Unk attr -> var_intf attr end in
  loop e

let intdom2finter a = intsI (Attr.min a) (Attr.max a)

let eval_exp = eval FloatAttr.dom intdom2finter

let k_pi vx =
  let k = divIII vx piI in
  let k_min =
    if min_val vx < 0. then truncate (min_val k) -1
    else truncate (min_val k) in
  let k_max =
    if max_val vx < 0. then truncate (max_val k) -1
    else truncate (max_val k) in
  mulIII (floatsI (float k_min) (float k_max)) piI

let minus x y = Bin (Minus, x, y)

let float2fd_refine x float_dom =
  let (miniv, maxiv) = int_bounds float_dom in
  if miniv > maxiv then
    Fcl_stak.fail
      "Fcl_float_cstr.float2fd_refine: reduce_dom over int : Unk(x)"
  else
    Fd.refine_low_up x miniv maxiv

let fbounds2ints left right =
  let miniv =
    let (fm, im) = modf left in
    if  im < 0. || fm = 0. then truncate im else truncate im + 1
  and maxiv =
    let (fmax, imax) = modf right in
    if  imax > 0. || fmax = 0. then truncate imax else truncate imax - 1 in
  (miniv, maxiv)

let get_val = function
    Cstv x -> x
  | Varv (_, v) -> v
  | Var_intv (_, v) -> v
  | Unv(_, _, v) -> v
  | Binv(_, _, _, v) -> v

(* [get_var_list exp] returns a pair of lists (float_vars, fd_vars)
   of variables of [exp] *)
let get_var_list exp =
  let rec loop l1 l2 = function
      Un (_, x) -> loop l1 l2 x
    | Bin (_, x1, x2) ->
	let (la, lb) = loop l1 l2 x1 in
	loop la lb x2
    | Var x ->
	begin match FloatVar.value x with
	  Val v -> (l1, l2)
	| Unk a -> (x :: l1, l2) end
    | Var_int x -> begin
	match Fd.value x with
	  Val v -> (l1, l2)
	| Unk a -> (l1, x :: l2)  end
    | Cst _ -> (l1, l2) in
  loop [] [] exp

let singletonise comp l =
  let rec iter l =
    match l with
      x1 :: ((x2 :: xs) as x2xs) -> 
	if comp x1 x2 = 0 then iter x2xs
	else x1 :: iter x2xs
    | [x] -> [x]
    | [] -> [] in
  iter (List.sort comp l)

let vars_of_exp exp =
  let (floats, fds) = get_var_list exp in
  let floats = singletonise FloatVar.compare floats
  and fds = singletonise Fd.compare fds in
  (floats, fds)

let simplify varf var_intf e =
  let rec loop e =
  match e with
      Cst c -> Cst c
    | Var var -> begin
	match FloatVar.value var with
	  Val v -> Cst v
	| Unk a -> varf e a end
    | Var_int var -> begin
	match Fd.value var with
	  Val v -> Cst (FI.intiI v)
	| Unk a -> var_intf e a end
    | Un (op, x) -> begin
	match loop x  with
	  Cst c -> Cst (eval_un_op op c)
	| other -> Un (op, other) end
    | Bin (op, x1, x2) -> begin
	let sx1 = loop x1 and sx2 = loop x2 in
	match (sx1, sx2) with
	  (Cst c1, Cst c2) -> Cst (eval_bin_op op c1 c2)

	| (Cst c1, Bin (op2, Cst c21, x22)) -> begin
	    match (op, op2) with
	      (Plus, Plus)->
		Bin (Plus, Cst (eval_bin_op Plus c1 c21), x22)
	    | (Plus, Minus) ->
		Bin (Minus, Cst (eval_bin_op Plus c1 c21), x22)
	    | (Minus, Plus) ->
		Bin (Minus, Cst (eval_bin_op Minus c1 c21), x22)
	    | (Minus, Minus) ->
		Bin (Plus, Cst (eval_bin_op Minus c1 c21), x22)
	    | (Mult, Mult) ->
		Bin (Mult, Cst (eval_bin_op Mult c1 c21), x22)
	    | (Mult, Div) ->
		Bin (Div, Cst (eval_bin_op Mult c1 c21), x22)
	    | (Div, Mult) ->
		Bin (Div, Cst (eval_bin_op Div c1 c21), x22)
	    | (Div, Div) -> Bin (Mult, Cst (eval_bin_op Div c1 c21), x22) 
	    | _ -> Bin (op, sx1, sx2) end

	| (Cst c1, Bin (op2, x21, Cst c22)) -> begin
	    match (op, op2) with
	      (Plus, Plus)->
		Bin (Plus, x21, Cst (eval_bin_op Plus c1 c22))
	    | (Plus, Minus) ->
		Bin (Plus, x21, Cst (eval_bin_op Minus c1 c22))
	    | (Minus, Plus) ->
		Bin (Minus, x21, Cst (eval_bin_op Minus c1 c22))
	    | (Minus, Minus) ->
		Bin (Minus, Cst (eval_bin_op Plus c1 c22), x21)
	    | (Mult, Mult) ->
		Bin (Mult, x21, Cst (eval_bin_op Mult c1 c22))
	    | (Mult, Div) ->
		Bin (Mult, x21, Cst (eval_bin_op Div c1 c22))
	    | (Div, Mult) ->
		Bin (Div, Cst (eval_bin_op Div c1 c22), x21)
	    | (Div, Div) -> Bin (Div, Cst (eval_bin_op Mult c1 c22), x21) 
	    | _ -> Bin (op, sx1, sx2) end

	| (Bin (op1, Cst c11, x12), Cst c2) -> begin
	    match (op, op1) with
	      (Plus, Plus) ->
		Bin (Plus, x12, Cst (eval_bin_op Plus c11 c2))
	    | (Plus, Minus) ->
		Bin (Minus, Cst (eval_bin_op Plus c11 c2), x12)
	    | (Minus, Plus) ->
		Bin (Plus, Cst (eval_bin_op Minus c11 c2), x12)
	    | (Minus, Minus) ->
		Bin (Minus, Cst (eval_bin_op Minus c11 c2), x12)
	    | (Mult, Mult) ->
		Bin (Mult, x12, Cst (eval_bin_op Mult c11 c2))
	    | (Mult, Div) ->
		Bin (Div, Cst (eval_bin_op Mult c11 c2), x12)
	    | (Div, Mult) ->
		Bin (Mult, x12, Cst (eval_bin_op Div c11 c2))
	    | (Div, Div) -> Bin (Div, Cst (eval_bin_op Div c11 c2), x12) 
	    | _ -> Bin (op, sx1, sx2) end

	| (Bin (op1, x11, Cst c12), Cst c2) -> begin
	    match (op, op1) with
	      (Plus, Plus)->
		Bin (Plus, x11, Cst (eval_bin_op Plus c12 c2))
	    | (Plus, Minus) ->
		Bin (Plus, x11, Cst (eval_bin_op Minus c2 c12))
	    | (Minus, Plus) ->
		Bin (Plus, x11, Cst (eval_bin_op Minus c12 c2))
	    | (Minus, Minus) ->
		Bin (Minus, x11, Cst (eval_bin_op Plus c12 c2))
	    | (Mult, Mult) ->
		Bin (Mult, x11, Cst (eval_bin_op Mult c12 c2))
	    | (Mult, Div) ->
		Bin (Mult, x11, Cst (eval_bin_op Div c2 c12))
	    | (Div, Mult) ->
		Bin (Mult, x11, Cst (eval_bin_op Div c12 c2))
	    | (Div, Div) -> Bin (Div, x11, Cst (eval_bin_op Mult c12 c2))
	    | _ -> Bin (op, sx1, sx2) end

	| _ -> Bin(op, sx1, sx2)
    end in
  loop e

let fst_arg x _ = x
let cstr_simplify = simplify fst_arg fst_arg

let simplify_ref exp_ref =
  Fcl_stak.set exp_ref (cstr_simplify (Fcl_stak.get exp_ref))


(***************************************************************)
(* x = y specialization *)
let equal_vars x y =
  let name = "Fcl_float_cstr.equal_vars" in
  let delay c = 
    FloatVar.delay [FloatVar.on_refine] x c;
    FloatVar.delay [FloatVar.on_refine] y c in
  let update _ =
    Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - before update: %a = %a\n" name FloatVar.fprint x FloatVar.fprint y);
    let result =
      match (FloatVar.value x, FloatVar.value y) with
	Val x, Val y -> if FI.is_empty (FI.intersect x y) then Fcl_stak.fail name; true
      | Val x, Unk _ -> FloatVar.unify y x; true
      | Unk _, Val y -> FloatVar.unify x y; true
      | Unk xa, Unk ya -> begin
	  let inter_ = intersect (FloatAttr.dom xa) (FloatAttr.dom ya) in
	  FloatVar.refine x inter_;
	  FloatVar.refine y inter_;
	  FloatVar.is_bound x end in
    Fcl_debug.call 'f' (fun s -> Printf.fprintf s "%s - after update: %a = %a\n" name FloatVar.fprint x FloatVar.fprint y);
    result in
  let fprint chout =
    Printf.fprintf chout "%s: %a = %a"
      name FloatVar.fprint x FloatVar.fprint y in
  C.create ~name ~fprint update delay


(***************************************************************)
(*            HC4 reduction function                           *)
(*                                                             *)
(*   Function that reduce domains of exp,                      *)
(*   knowing that exp must be evaluated to v                   *)
(*                                                             *)
(***************************************************************)
let rec hull_eval exp =
  match exp with
    Cst x -> (Cstv x, x)
  | Un (op, x) ->
      let (x1, v1) = hull_eval x in
      let nv1 = eval_un_op op v1 in
      (Unv (op, x1, nv1), nv1)
  | Var x -> begin
      match FloatVar.value x with
	Val v -> (Varv (x, v), v)
      | Unk a -> let vx = FloatAttr.dom a in (Varv (x, vx), vx)end
  | Var_int x -> begin
      match Fd.value x with
	Val v -> let dom_x = intiI v in (Var_intv (x, dom_x), dom_x)
      | Unk a -> 
	  let vx = intsI (Attr.min a) (Attr.max a) in
	  (Var_intv (x, vx), vx) end
  | Bin (op, exp1, exp2) ->
      let (e1, v1) = hull_eval exp1 and (e2, v2) = hull_eval exp2 in
      let v = eval_bin_op op v1 v2 in
      (Binv (op, e1, e2, v), v)
	

let create_simplify exp_ref float_vars fd_vars =
  let name = "Fcl_float_cstr.simplify" in
  let update  _ =
    if included zero (eval_exp (Fcl_stak.get exp_ref)) then begin
      Fcl_stak.set exp_ref (cstr_simplify (Fcl_stak.get exp_ref));
      false end
    else
      Fcl_stak.fail (Printf.sprintf "%s: constraint violated" name) in

  let delay c =
    List.iter (fun x -> FloatVar.delay [FloatVar.on_subst] x c) float_vars;
    List.iter (fun x -> Fd.delay [Fd.on_subst] x c) fd_vars in

  C.create ~name ~priority:C.immediate update delay


let hull_reduce name exp v =
  let rec reduce_dom exp v =
    match exp with
      Cstv x -> 
	if is_empty (intersect x v) then
	  Fcl_stak.fail (name ^ ": reduce_dom over Cstv(x)")
	else true
    | Varv (x, vx) -> begin 
	match FloatVar.value x with
	  Val x -> 
	    if is_empty (intersect x v) then
	      Fcl_stak.fail (name ^ ": reduce_dom over Val(x)")
	    else true
	| Unk ax ->
	    let vx = FloatAttr.dom ax in 
	    let inter_v_vx = intersect v vx in
	    if is_empty inter_v_vx then
	      Fcl_stak.fail (name ^ ": reduce_dom over Unk(x)")
	    else begin
	      FloatVar.refine x inter_v_vx;
	      FloatVar.is_bound x
	      (*false*) end end
    | Var_intv(x, vx) -> begin
	match Fd.value x with
	  Val x ->
	    if is_empty (intersect (intiI x) v) then
	      Fcl_stak.fail (name ^ ": reduce_dom over Val(x)")
	    else true
	| Unk ax ->
	    let inter_v_vx = intersect v vx in
	    if is_empty inter_v_vx then
	      Fcl_stak.fail (name ^ ": reduce_dom over Unk(x)")
	    else begin
	      float2fd_refine x inter_v_vx;
	      Fd.is_bound x
	      (*false*) end end
    | Unv (op, x, vop) -> begin
	let vx = get_val x in
	let new_vop = intersect vop v in
	match op with 
	  Abs ->  
	    if min_val vx > 0. then reduce_dom x new_vop
	    else if max_val vx < 0. then reduce_dom x (negII new_vop)
	    else reduce_dom x (union new_vop (negII new_vop))
	| Neg -> reduce_dom x (negII new_vop)
	| Sqrt -> reduce_dom x (sqrII new_vop)
	| Sqr -> 
	    let sq = sqrtII new_vop in
	    if min_val vx > 0. then reduce_dom x (intersect vx sq)
	    else if max_val vx < 0. then reduce_dom x (negII sq)
	    else reduce_dom x (union sq (negII sq))
	| Exp -> reduce_dom x (logII new_vop)
	| Log -> reduce_dom x (expII new_vop)
	| Sin -> 
	    let k_2pi_ = k_2pi (addIII vx half_pi) in
	    let asin_new_vop = asinII new_vop in
	    reduce_dom x 
	      (union 
		 (intersect vx
		    (addIII k_2pi_ asin_new_vop))
		 (intersect vx
		    (subIII (addIII k_2pi_ piI) asin_new_vop)))
	| Cos ->
	    let k_2pi_ = k_2pi vx in
	    let acos_new_vop = acosII new_vop in
	    reduce_dom x
	      (union
		 (intersect vx
		    (addIII k_2pi_ acos_new_vop))
		 (intersect vx
		    (subIII (addIII k_2pi_ two_pi) acos_new_vop)))
	| Tan -> 
	    let k_pi_ =  k_pi (addIII vx half_pi) in
	    let atan_new_vop = atanII new_vop in
	    reduce_dom x (addIII k_pi_ atan_new_vop)
	| Cot -> 
	    let k_pi_ =  k_pi vx in
	    let cot_new_vop = acotII new_vop in
	    reduce_dom x (addIII k_pi_ cot_new_vop)
	| Asin -> reduce_dom x (sinII new_vop)
	| Acos -> reduce_dom x (cosII new_vop)
	| Atan -> reduce_dom x (tanII new_vop)
	| Acot -> reduce_dom x (cotII new_vop)
	| Pow i -> begin
	    match i with
	      0 -> true
	    | 1 -> reduce_dom x new_vop
	    | 2 ->
		let sq = sqrtII (absII new_vop) in
		if min_val(vx) > 0. then reduce_dom x sq
		else if max_val(vx) < 0. then reduce_dom x (negII sq)
		else reduce_dom x (union sq  (negII sq))
	    | _ ->
		let pow_abs =
		  powIII (absII new_vop) (divIII (intiI 1) (intiI i)) in
		if min_val vx > 0. then reduce_dom x pow_abs
		else if max_val vx < 0. then reduce_dom x (negII pow_abs)
		else reduce_dom x (union pow_abs (negII pow_abs)) end end
	  
    | Binv (binop, x1, x2, vx) -> begin
	let vx1 = get_val x1 and vx2 = get_val x2 in
	let new_vx = intersect vx v in
	match  binop with
	  Plus ->   
            let red_x1 = reduce_dom x1 (subIII new_vx vx2) in
	    let red_x2 = reduce_dom x2 (subIII new_vx vx1) in
	    red_x1 && red_x2
	| Minus ->
	    let red_x1 = reduce_dom x1 (addIII new_vx vx2) in
	    let red_x2 = reduce_dom x2 (subIII vx1 new_vx) in
	    red_x1 && red_x2
	| Mult ->
	    let red_x1 =  reduce_dom x1 (divIII new_vx vx2) in
	    let red_x2 =  reduce_dom x2 (divIII new_vx vx1) in
	    red_x1 && red_x2
	| Div ->
	    let red_x1 =  reduce_dom x1 (mulIII new_vx vx2) in
	    let red_x2 =  reduce_dom x2 (divIII vx1 new_vx) in
	    red_x1 && red_x2 end in
  let (e, _) = hull_eval exp in
  reduce_dom e v

(**********  Equality constraint using HC4 narrowing algorithm  *********)

let nope () = false

(*
let hc4_equal exp =
  let exp_ref = Fcl_stak.ref exp in
  let name = "Fcl_float_cstr.hc4_equal" in

  let update _ = hull_reduce name (Fcl_stak.get exp_ref) zero in
 
 let (float_vars, fd_vars) = vars_of_exp exp in

 let delay c =
   List.iter (fun x -> FloatVar.delay [FloatVar.on_refine] x c) float_vars;
   List.iter (fun x -> Fd.delay [Fd.on_refine] x c) fd_vars in

 C.conjunction
   [C.create ~name ~priority:C.normal ~init:nope update delay;
    C.create ~name ~priority:C.later ~init:nope update delay;
    create_simplify exp_ref float_vars fd_vars]
*)

(* simplified hc4 version *)
let hc4_equal exp =
  let exp_ref = Fcl_stak.ref exp in
  let name = "Fcl_float_cstr.hc4_equal" in

  let update _ =
    Fcl_debug.call 'h' (fun s -> Printf.fprintf s "%s - before update: %a <= 0\n" name fprint_exp (Fcl_stak.get exp_ref));
    simplify_ref exp_ref;
    let result = hull_reduce name (Fcl_stak.get exp_ref) zero in
    Fcl_debug.call 'h' (fun s -> Printf.fprintf s "%s - after update: %a <= 0\n" name fprint_exp (Fcl_stak.get exp_ref));
    result in
 
  let (float_vars, fd_vars) = vars_of_exp exp in

  let delay c =
   List.iter (fun x -> FloatVar.delay [FloatVar.on_refine] x c) float_vars;
   List.iter (fun x -> Fd.delay [Fd.on_refine] x c) fd_vars in

  let fprint chout =
    Printf.fprintf chout "%s: %a" name fprint_exp (Fcl_stak.get exp_ref) in

 C.create ~name ~fprint update delay



(*********   Inequality constraint using HC4 narrowing algorithm  ******) 
(*
let hc4_le exp =
  let exp_ref  = Fcl_stak.ref exp in
  let name = "Fcl_float_cstr.hc4_le" in

  let update _ = hull_reduce name (Fcl_stak.get exp_ref) negative in

  let (float_vars, fd_vars) = vars_of_exp exp in

  let delay c =
    List.iter (fun x -> FloatVar.delay [FloatVar.on_refine] x c) float_vars;
    List.iter (fun x -> Fd.delay [Fd.on_refine] x c) fd_vars in

  C.conjunction
    [C.create ~name ~init:nope update delay;
     create_simplify exp_ref float_vars fd_vars]
*)

(* simplified hc4_le version *)
let hc4_le exp =
  let exp_ref  = Fcl_stak.ref exp in
  let name = "Fcl_float_cstr.hc4_le" in

  let (float_vars, fd_vars) = vars_of_exp exp in

  let delay c =
    List.iter
      (fun x -> FloatVar.delay [FloatVar.on_refine] x c) float_vars;
    List.iter (fun x -> Fd.delay [Fd.on_refine] x c) fd_vars in

  let update _ =
    Fcl_debug.call 'h' (fun s -> Printf.fprintf s "%s - before update: %a <= 0\n" name fprint_exp (Fcl_stak.get exp_ref));
    simplify_ref exp_ref;
    let is_satisfied =
      hull_reduce name (Fcl_stak.get exp_ref) negative in
    Fcl_debug.call 'h' (fun s -> Printf.fprintf s "%s - after update: %a <= 0\n" name fprint_exp (Fcl_stak.get exp_ref));
    is_satisfied in

  C.create ~name update delay


let is_boolean_var v =
  match Fcl_var.Fd.value v with
    Val c -> c = 0 || c = 1
  | Unk _ -> Fcl_var.Fd.min v = 0 && Fcl_var.Fd.max v = 1

let hc4_is_le_cstr exp b =
  assert (is_boolean_var b);
  let name = "Fcl_float_cstr.hc4_is_le" in
  let tmp = FloatVar.create (eval_exp exp) in

  let delay c =
    FloatVar.delay [FloatVar.on_refine] tmp c;
    Fd.delay [Fd.on_refine] b c in
  
  let update _ =
    Fcl_debug.call 'h' (fun s -> Printf.fprintf s "%s - before update: (%a; %a)\n" name FloatVar.fprint tmp Fcl_var.Fd.fprint b);
    let is_satisfied =
      match (Fcl_var.Fd.value b, FloatVar.value tmp) with
	(Val vb, Val vtmp) ->
	  if (vb = 1 && FI.max_val vtmp <= 0.) ||
	  (vb = 0 && FI.min_val vtmp >= 0.) then true
	  else Fcl_stak.fail (name ^ ": violated with instantiated values")
      | (Val vb, Unk a) ->
	  if vb = 1 then
	    FloatVar.refine tmp (remove_up zero_elt (FloatAttr.dom a))
	  else
  	    FloatVar.refine tmp (remove_low zero_elt (FloatAttr.dom a));
	  true
      | (Unk a, Val vtmp) ->
	  if FI.max_val vtmp <= 0. then begin
	    Fcl_var.Fd.unify b 1;
	    true end
	  else if FI.min_val vtmp > 0. then begin
	    Fcl_var.Fd.unify b 0;
	    true end
	  else false
      | (Unk ab, Unk atmp) ->
	  let da = FloatAttr.dom atmp in
	  if max_val da <= 0. then begin
	    Fcl_var.Fd.unify b 1;
	    true end
	  else if min_val da > 0. then begin
	    Fcl_var.Fd.unify b 0;
	    true end
	  else false in
    Fcl_debug.call 'h' (fun s -> Printf.fprintf s "%s - after update: (%a; %a)\n" name FloatVar.fprint tmp Fcl_var.Fd.fprint b);
    is_satisfied in

  let fprint chout =
    Printf.fprintf chout "%s: %a < 0 <=> %a" name FI.fprint (eval_exp exp) Fd.fprint b in

  let tmp_equal_exp =
    (*match exp with
      Bin (Minus, Var x, Cst z) when is_zero z  -> equal_vars tmp x
    | _ ->*) hc4_equal (Bin (Minus, Var tmp, exp)) in

  C.conjunction [tmp_equal_exp; C.create ~name ~fprint update delay]


let hc4_is_le exp =
  let b = Fcl_var.Fd.create ~name:"is_le" (Fcl_domain.interval 0 1) in
  Fcl_cstr.post (hc4_is_le_cstr exp b);
  b



(***********************************************************************)
(*                                                                     *)
(*                BC3 Monovariate Reduction Functions                  *)
(*          Reduces x to be box-consistent according to e              *)
(*                                                                     *)
(***********************************************************************)         

let rec derive exp dom i = 
  match exp with
    Cst c -> (c, zero)
  | Var var -> 
      begin match FloatVar.value var with
	Val v -> (v, zero)
      | Unk a -> 
	  let da = FloatAttr.dom a in
	  if FloatAttr.id a = i then (dom, one) else (dom, zero) end
  | Var_int var ->
      begin match Fd.value var with
	Val v -> (intiI v, zero)
      | Unk a -> 
	  let fda = intsI (Attr.min a) (Attr.max a) in
	  if Attr.id a = i then (dom, one) else 
	  let fda = intsI (Attr.min a) (Attr.max a) in (fda, zero) end
  | Un (op, expr) ->
      let (v, vd) = derive expr dom i in
      begin match op with
	Abs ->
	  if min_val v >= 0. then (v, vd) 
	  else if max_val v < 0. then (negII v, negII vd) 
	  else (absII v, mulIII (floatsI (-1.) 1.) vd)
      | Pow i -> (powII v i, mulIII vd (mulIII (intiI i) (powII v (i-1))))
      | Neg  -> (negII v, negII vd)
      | Sqrt ->
	  let sqrt_v = sqrtII v in 
	  (sqrt_v, divIII vd (mulIII two sqrt_v))
      | Sqr -> (sqrII v, mulIII two (mulIII v vd))
      | Exp -> let exp_v = expII v in (exp_v, mulIII vd exp_v)
      | Log -> (logII v, divIII vd v)
      | Sin -> (sinII v, mulIII vd (cosII v))
      | Cos -> (cosII v, mulIII vd (negII (sinII v)))
      | Tan ->
	  let tan_v = tanII v in
	  (tan_v, mulIII vd (addIII one (sqrII tan_v)))
      | Cot ->
	  let cot_v = cotII v in
	  (cot_v, mulIII (negII vd) (addIII one (sqrII cot_v)))
      | Asin -> (asinII v, divIII vd (sqrII (subIII one (sqrII v))))
      | Acos -> (acosII v, divIII (negII vd) (sqrtII (subIII one (sqrII v))))
      | Atan -> (atanII v, divIII vd (addIII one (sqrII v)))
      | Acot -> (acotII v, divIII (negII vd) (addIII one (sqrII v))) end
  | Bin(op,e1,e2) -> 
      let (v1,d1) = derive e1 dom i in
      let (v2,d2) = derive e2 dom i in
      begin match op with
	Plus -> (addIII v1 v2, addIII d1 d2)
      | Minus ->(subIII v1 v2, subIII d1 d2)
      | Mult -> (mulIII v1 v2, addIII (mulIII v1 d2) (mulIII v2 d1))
      | Div ->
	  (divIII v1 v2,
	   subIII (divIII d1 v2) (divIII (mulIII d2 v1) (sqrII v2))) end



(***********************************************************************)
(* BC3 *)
let box_simplify e i =
  let varf e a =
    if FloatAttr.id a = i then e else Cst (FloatAttr.dom a)
  and var_intf e a =
    if Attr.id a = i then e else Cst (intsI (Attr.min a) (Attr.max a)) in
  simplify varf var_intf e

let eval_monovariate exp di i =
  let rec iter = function
      Cst x -> x
    | Un (op, x) -> eval_un_op op (iter x)
    | Bin (op, exp1, exp2) -> eval_bin_op op (iter exp1) (iter exp2)
    | Var x -> begin 
	match FloatVar.value x with
	  Val v -> v
	| Unk a -> 
	    if FloatAttr.id a = i then di else FloatAttr.dom a end
    | Var_int x -> begin
	match Fd.value x with
	  Val v -> intiI v
	| Unk a -> 
	    if Attr.id a = i then di
	    else intsI (Attr.min a) (Attr.max a) end in
  iter exp

let eval_monovariate exp di i =
  let varf a =
    if FloatAttr.id a = i then di else FloatAttr.dom a
  and var_intf a =
    if Attr.id a = i then di else intdom2finter a in
  eval varf var_intf exp


let eval_taylor exp di i =
  let midi = mid di in
  let (f, df) = derive exp di i in
  intersect
    f (addIII (eval_monovariate exp midi i) (mulIII df (subIII di midi)))
    
let newt f dom i =
  let rec newt_iter dom df_dom =
    let mid_ = mid dom in
    let new_di =
      intersect
	dom (subIII mid_ (divIII (eval_monovariate f mid_ i) df_dom)) in
    if constant_delta dom new_di then
      let (_, df_ndom) = derive f new_di i in
      newt_iter new_di df_ndom
    else dom in
  let (_, df_dom) = derive f dom i in
  if included zero df_dom then dom else newt_iter dom df_dom

let is_nan d =
  match (classify_float (min_val d), classify_float (max_val d)) with
    (FP_nan, FP_nan) -> true
  | _ -> false

let rec l_nar f i = function
    [] -> empty
  | dom :: xs -> 
      let f_i = eval_monovariate f dom i in 
      if included zero f_i then 
	let min_ = min_val dom in
	(* takes into account the width of the domain, not only
	   the precision *)
	let borne = delta_width dom in
	let min_plus = intersect (floatsI min_ (min_ +. borne)) dom in
	let min_plus_eval = eval_monovariate f min_plus i in
	Fcl_debug.call 'f' (fun s -> Printf.fprintf s "l_nar (min_plus): %a\n" fprint min_plus_eval);
	if included zero min_plus_eval then dom
	else
	  let newt_dom = newt f (floatsI (min_ +. borne) (max_val dom)) i in
	  Fcl_debug.call 'f' (fun s -> Printf.fprintf s "l_nar (newt_dom): %a\n" fprint newt_dom);
	  let mid_ = mid newt_dom in
	  l_nar f i
	    (remove_up  mid_ newt_dom :: remove_low  mid_ newt_dom :: xs)
      else l_nar f i xs

let rec r_nar f i = function
    [] -> empty
  | dom :: xs ->
      let f_i = eval_monovariate f dom i in 
      if included zero f_i then 
	let max_ = max_val dom in
	let borne = delta_width dom in
	let minus_max = intersect (floatsI (max_ -. borne) max_) dom in
	let minus_max_eval = eval_monovariate f minus_max i in
	Fcl_debug.call 'f' (fun s -> Printf.fprintf s "r_nar (min_plus): %a\n" fprint minus_max_eval);
	if included zero minus_max_eval then dom
	else
	  let newt_dom = newt f (floatsI (min_val dom) (max_ -. borne)) i in
	  Fcl_debug.call 'f' (fun s -> Printf.fprintf s "r_nar (newt_dom): %a\n" fprint newt_dom);
	  let mid_ = mid newt_dom in
	  r_nar f i
	    (remove_low  mid_ newt_dom :: remove_up  mid_ newt_dom :: xs)
      else r_nar f i xs

let box_narrow name f_ref x () =
  let f_val = Fcl_stak.get f_ref in
  if not (included zero (eval_exp f_val)) then
    Fcl_stak.fail (name ^ ": Var x -> box_reduce (start)") 
  else
    match x with
      Float var -> begin
	match FloatVar.value var with
	  Val v -> true	   
	| Unk a -> 
	    let i = FloatAttr.id a in
	    let f = box_simplify f_val i in
	    let da = FloatAttr.dom a in
	    let zg = l_nar f i [da] in
	    if is_nan zg then false else
	    if is_empty zg then
	      (*Printf.printf "box_narrow: %a = %a %a (zg=%a)\n%!" fprint_exp f_val fprint (eval_exp f_val) FloatVar.fprint var fprint zg;*)
	      Fcl_stak.fail
		(name ^ ": Var x-> box_reduce over Unk float var (zg)")
	    else
	      let zg_min = min zg in
	      let zd = r_nar f i [interval zg_min (max da)] in
	      if is_empty zd then
		Fcl_stak.fail
		  (name ^ ": Var x-> box_reduce over Unk float var (zd)")
	      else begin
		FloatVar.refine_low_up var zg_min (max zd);
		false end end
    | Int var -> begin
	match Fd.value var with
	  Val v -> true
	| Unk a ->
	    let i = Attr.id a in 
	    let f = box_simplify f_val i in
	    let a_min = Attr.min a and a_max = Attr.max a in
	    let dom_i = intsI a_min a_max in
	    let zg = l_nar f i [dom_i] in
	    if is_nan zg then false else
	    if is_empty zg then
	      Fcl_stak.fail
		(name ^ ": Var x-> box_reduce over Unk int var (zg)")
	    else
	      let zg_min = min_val zg in
	      let zd = r_nar f i [floatsI zg_min (max_val dom_i)] in
	      if is_empty zd then
		Fcl_stak.fail (name ^ ": Var x-> box_reduce over Unk int var (zd)")
	      else begin
		let zd_max = max_val zd in
		let (low, up) = fbounds2ints zg_min zd_max in
		Fcl_var.Fd.refine_low_up var low up;
		false end end

(* [occurences comp l] returns a pair of lists [(singles, multiples)] of
   elements of [l] respectively containing single and multiple occurences
   of the elements according to [comp] *)
let occurences comp l =
  let rec loop single = function
      [] -> ([], [])
    | [x] -> if single then ([x], []) else ([], [x])
    | x :: ((y :: xs) as yxs) ->
	if comp x y = 0 then loop false yxs
	else
	  let (ls, lm) = loop true yxs in
	  if single then (x :: ls, lm) else (ls, x :: lm) in
  loop true (List.sort comp l)

let fold_float name exp_ref x l =
  match FloatVar.value x with
    Val v -> l
  | Unk a -> box_narrow name exp_ref (Float x) :: l
   	
let fold_int name exp_ref x l =
  match Fd.value x with
    Val v -> l
  | Unk a -> box_narrow name exp_ref (Int x) :: l

(***
let bc3_equal exp =
  let exp_ref = Fcl_stak.ref (cstr_simplify exp) in
  let name = "float : bc3_equal" in 
  let (float_vars, fd_vars) = vars_of_exp exp in
 
    (*List.iter (fun x -> FloatVar.fprint stdout x;Printf.printf ";") float_vars;
    Printf.printf "\n";
    List.iter (fun x -> Fd.fprint stdout x;Printf.printf ";") fd_vars;
    Printf.printf "\n%!"; *)
  
  let delay_box c =
    List.iter (fun x -> FloatVar.delay [FloatVar.on_refine] x c) float_vars;
    List.iter (fun x -> Fd.delay [Fd.on_refine] x c) fd_vars in

  let box_list =
    List.fold_right
      (fold_float name exp_ref) float_vars
      (List.fold_right (fold_int name exp_ref) fd_vars []) in

  let update_box _ = 
   List.fold_right (fun f b -> f () && b) box_list true in
  
  let exp_l = 
    [C.create ~name ~priority:C.later ~init:nope update_box delay_box;
     C.create ~name ~priority:C.even_later ~init:nope update_box delay_box;
     create_simplify exp_ref float_vars fd_vars]
  in
  C.conjunction exp_l
***)

(* simplified bc3 version *)
let bc3_equal exp =
  let exp_ref = Fcl_stak.ref (cstr_simplify exp) in
  let name = "Fcl_float_cstr.bc3_equal" in 
  let (float_vars, fd_vars) = vars_of_exp exp in

  let delay_box c =
    List.iter
      (fun x -> FloatVar.delay [FloatVar.on_refine] x c) float_vars;
    List.iter (fun x -> Fd.delay [Fd.on_refine] x c) fd_vars in

  let box_list =
    List.fold_right
      (fold_float name exp_ref) float_vars
      (List.fold_right (fold_int name exp_ref) fd_vars []) in

  let update_box _ =
    Fcl_debug.call '3' (fun s -> Printf.fprintf s "%s - before update: %a = 0\n" name fprint_exp (Fcl_stak.get exp_ref));
    simplify_ref exp_ref;
    let result = List.fold_right (fun f b -> f () && b) box_list true in
    Fcl_debug.call '3' (fun s -> Printf.fprintf s "%s - after update: %a = 0\n" name fprint_exp (Fcl_stak.get exp_ref));
    result in

  let fprint chout =
    Printf.fprintf chout "%s: %a" name fprint_exp (Fcl_stak.get exp_ref) in

  C.create ~name ~fprint update_box delay_box

let is_zero z = min_val z = 0. && max_val z = 0.

let bc3_is_le_cstr exp b =
  assert (is_boolean_var b);
  let name = "Fcl_float_cstr.bc3_is_le" in
  let tmp = FloatVar.create (eval_exp exp) in

  let delay c =
    FloatVar.delay [FloatVar.on_refine] tmp c;
    Fd.delay [Fd.on_refine] b c in
  
  let update _ =
    Fcl_debug.call '3' (fun s -> Printf.fprintf s "%s - before update: (%a; %a)\n" name FloatVar.fprint tmp Fcl_var.Fd.fprint b);
    let is_satisfied =
      match (Fcl_var.Fd.value b, FloatVar.value tmp) with
	(Val vb, Val vtmp) ->
	  if (vb = 1 && FI.max_val vtmp <= 0.) ||
	  (vb = 0 && FI.min_val vtmp >= 0.) then true
	  else Fcl_stak.fail (name ^ ": violated with instantiated values")
      | (Val vb, Unk a) ->
	  if vb = 1 then
	    FloatVar.refine tmp (remove_up zero_elt (FloatAttr.dom a))
	  else
  	    FloatVar.refine tmp (remove_low zero_elt (FloatAttr.dom a));
	  true
      | (Unk a, Val vtmp) ->
	  if FI.max_val vtmp <= 0. then begin
	    Fcl_var.Fd.unify b 1;
	    true end
	  else if FI.min_val vtmp > 0. then begin
	    Fcl_var.Fd.unify b 0;
	    true end
	  else false
      | (Unk ab, Unk atmp) ->
	  let da = FloatAttr.dom atmp in
	  if max_val da <= 0. then begin
	    Fcl_var.Fd.unify b 1;
	    true end
	  else if min_val da > 0. then begin
	    Fcl_var.Fd.unify b 0;
	    true end
	  else false in
    Fcl_debug.call '3' (fun s -> Printf.fprintf s "%s - after update: (%a; %a)\n" name FloatVar.fprint tmp Fcl_var.Fd.fprint b);
    is_satisfied in

  let tmp_equal_exp =
    (*match exp with
      Bin (Minus, Var x, Cst z) when is_zero z  -> equal_vars tmp x
    | _ ->*) bc3_equal (Bin (Minus, Var tmp, exp)) in

  C.conjunction [tmp_equal_exp; C.create ~name update delay]

let bc3_is_le exp =
  let b = Fcl_var.Fd.create ~name:"is_le" (Fcl_domain.interval 0 1) in
  Fcl_cstr.post (bc3_is_le_cstr exp b);
  b


(****************************************************************)
(* BC4 *)

let bc4_equal ?(unused = Fcl_stak.ref false) exp =
    
  let name = "float:BC4_equal" in
  let exp_ref = Fcl_stak.ref (cstr_simplify exp) in
  let (l1, l2) = get_var_list exp in
  let (h_l1, b_l1) = occurences FloatVar.compare l1 in
  let (h_l2, b_l2) = occurences Fd.compare l2 in

  let all_l1 = b_l1 @ h_l1 in
  let all_l2 = b_l2 @ h_l2 in
  let delay_box c = 
    List.iter (fun x -> FloatVar.delay [FloatVar.on_refine] x c) all_l1;
    List.iter (fun x -> Fd.delay [Fd.on_refine] x c) all_l2 in

  let list_update = 
    List.fold_right
      (fold_float name exp_ref)
      b_l1 (List.fold_right (fold_int name exp_ref) b_l2 []) in
  let box_update _ = 
    Fcl_stak.get unused ||
    List.fold_right (fun f b -> f () && b) list_update true in
  let hull_update _ =
    Fcl_stak.get unused || hull_reduce name (Fcl_stak.get exp_ref) zero in
  let delay_hull c = 
    List.iter (fun x -> FloatVar.delay [FloatVar.on_refine] x c) h_l1;
    List.iter (fun x -> Fd.delay [Fd.on_refine] x c) h_l2 in
  
  let exp_l =
    [C.create ~name ~priority:C.later  ~init:nope box_update delay_box;
     create_simplify exp_ref (h_l1 @ b_l1) (h_l2 @ b_l2);
     C.create ~name ~priority:C.normal ~init:nope hull_update delay_hull;
     C.create ~name ~priority:C.later ~init:nope hull_update delay_box;
     C.create ~name ~priority:C.even_later ~init:nope box_update delay_box] in

  C.conjunction exp_l
		
let bc4_le exp = bc4_equal (Bin (Plus, exp, Cst positive))


let bc4_is_le_cstr exp b =
  assert (is_boolean_var b);
  let unused = Fcl_stak.ref false in
  let name = "Fcl_float_cstr.bc4_is_le" in
  let tmp = FloatVar.create (eval_exp exp) in

  let delay c =
    FloatVar.delay [FloatVar.on_refine] tmp c;
    Fd.delay [Fd.on_refine] b c in
  
  let update _ =
    Fcl_debug.call '4' (fun s -> Printf.fprintf s "%s - before update: (%a; %a)\n" name FloatVar.fprint tmp Fcl_var.Fd.fprint b);
    let is_satisfied =
      match (Fcl_var.Fd.value b, FloatVar.value tmp) with
	(Val vb, Val vtmp) ->
	  if (vb = 1 && FI.max_val vtmp <= 0.) ||
	  (vb = 0 && FI.min_val vtmp >= 0.) then begin
	    Fcl_stak.set unused true;
	    true end
	  else Fcl_stak.fail (name ^ ": violated with instantiated values")
      | (Val vb, Unk a) ->
	  if vb = 1 then
	    FloatVar.refine tmp (remove_up zero_elt (FloatAttr.dom a))
	  else
  	    FloatVar.refine tmp (remove_low zero_elt (FloatAttr.dom a));
	  true
      | (Unk a, Val vtmp) -> 
	  if FI.max_val vtmp <= 0. then begin
	    Fcl_var.Fd.unify b 1;
	    Fcl_stak.set unused true;
	    true end
	  else if FI.min_val vtmp > 0. then begin
	    Fcl_var.Fd.unify b 0;
	    Fcl_stak.set unused true;
	    true end
	  else false
      | (Unk ab, Unk atmp) -> 
	  let da = FloatAttr.dom atmp in
	  if max_val da <= 0. then begin
	    Fcl_var.Fd.unify b 1;
	    Fcl_stak.set unused true;
	    true end
	  else if min_val da > 0. then begin
	    Fcl_var.Fd.unify b 0;
	    Fcl_stak.set unused true;
	    true end
	  else false in
    Fcl_debug.call '4' (fun s -> Printf.fprintf s "%s - after update: (%a; %a)\n" name FloatVar.fprint tmp Fcl_var.Fd.fprint b);
    is_satisfied in

  C.conjunction
    [bc4_equal ~unused (Bin (Minus, Var tmp, exp));
     C.create ~name update delay]

let bc4_is_le exp =
  let b = Fcl_var.Fd.create ~name:"is_le" (Fcl_domain.interval 0 1) in
  Fcl_cstr.post (bc4_is_le_cstr exp b);
  b


(****************   Default arithmetic constraints *********************) 

(* GROS PROBLEME : pour solve_with_return, si on utilise les nouvelles
   versions du max sans la specialisation [equal_vars], ca ne marche plus *)
let equal exp1 exp2 =
(***)  match (exp1, exp2) with
    (Var x, Var y) -> equal_vars x y
  | (Bin (Minus, Var x, Var y), Cst z)
  | (Cst z, Bin (Minus, Var x, Var y)) when is_zero z -> equal_vars x y
  | _ ->(***)
      (*bc4_equal (minus exp1 exp2)*)
      C.conjunction
	[hc4_equal (minus exp1 exp2); bc3_equal (minus exp1 exp2)]
  (*hc4_equal (minus exp1 exp2)*)
  (*bc3_equal (minus exp1 exp2)*) (* best for solve_with_return *)

(*** let le = bc4_le ***)
let le exp1 exp2 = hc4_le (minus exp1 exp2)

(***
(***)let is_le exp1 exp2 = bc4_is_le (minus exp1 exp2)(***)
(***)let is_le_cstr exp1 exp2 = bc4_is_le_cstr (minus exp1 exp2)(***)
***)
(*** GROSSE BUG pour solve_with_return avec bc3_is_le ***)
(***
let is_le exp1 exp2 = bc3_is_le (minus exp1 exp2)(***)
let is_le_cstr exp1 exp2 = bc3_is_le_cstr (minus exp1 exp2)
***)

(***)
let is_le exp1 exp2 = hc4_is_le (minus exp1 exp2)
let is_le_cstr exp1 exp2 = hc4_is_le_cstr (minus exp1 exp2)
(***)


(********************* Fonction eq_or_eq*******************************)
(**** constraint x1 = y1 || x2 = y2 qui effectue un filtrage **********)
(*

let eq_or_eq_cstr x1 y1 x2 y2 =
 let xminy1 = Bin(Minus,x1,y1) in
 let xminy2 = Bin(Minus,x2,y2) in
 let b1 = is_le x1 y1 in
 let b2 = is_le x2 y2 in
 let name = "or cstr" in
 let tmp1 = FloatVar.create ~name:"tmp1" (eval_exp xminy1) in
 C.post(bc4_equal(Bin(Minus,Var tmp1, xminy1)));
 
 let tmp2 = FloatVar.create ~name:"tmp2" (eval_exp xminy2) in
 C.post(bc4_equal(Bin(Minus,Var tmp2, xminy2))); 

 let (l11,l12) = get_var_list xminy1 in
 let (l21,l22) = get_var_list xminy2 in
 let (l1,l2) =
   (List.merge FloatVar.compare l11 l21,
    List.merge Fd.compare l12 l22)  in
 
 let delay c = 
   List.iter (fun x -> FloatVar.delay [FloatVar.on_refine] x ~waking_id:0 c) l1;
   List.iter (fun x -> Fd.delay [Fd.on_refine] x ~waking_id:0 c) l2;
   Fd.delay [Fd.on_refine] b1 ~waking_id:1 c;
   Fd.delay [Fd.on_refine] b2 ~waking_id:2 c;
 in
 
 let fold_float name exp1 exp2 x l = 
   match FloatVar.value x with
     Val v -> l
   | Unk a -> (box_or name exp1 exp2 (Float x))::l
 in
 let fold_int  name exp x l = 
   match Fd.value x with
     Val v -> l
   | Unk a -> (box_or name exp1 exp2 (Int x))::l 
 in
 
 
 let update wid =
   let list_update = List.fold_right 
       (fold_int "or" xminy2 xminy1) 
       l2
       (List.fold_right (fold_float "or" xminy2 xminy1) 
       l1)
   in

   if wid = 1 then 
     if Fd.elt_value b1 = 0 
     then (Cstr.post (fd2e b2 =~ i2e 1);true) 
     else true
   else
     if wid = 2 then 
       if Fd.elt_value b2 = 0 
       then (Cstr.post (fd2e b1 =~ i2e 1);true) 
       else true
     else
       (List.iter (fun f -> f ()) list_update; 
	false)
 in ()
*)


let bc4_equal = bc4_equal ~unused:(Fcl_stak.ref false)

let simplify = cstr_simplify
