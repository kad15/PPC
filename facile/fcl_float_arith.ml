open Fcl_float_cstr
type t = Fcl_float_cstr.t

let f2fe x = Cst (Fcl_float_domain.floatI x)
let dom2fe x = Cst x
let fv2fe x = Var x
let fd2fe x = Var_int x

let (+.~) x y = Bin (Plus, x, y)
let (-.~) x y = Bin (Minus, x, y)
let ( *.~ ) x y = Bin (Mult, x, y)
let (/.~) x y = Bin (Div, x, y)

let sum_fe a =
  let n = Array.length a in
  if n = 0 then
    Cst (Fcl_float_domain.floatI 0.)
  else begin
    let s = ref a.(0) in
    for i = 1 to Array.length a - 1 do
      s := a.(i) +.~ !s
    done;
    !s end

let sum_fv a = sum_fe (Array.map fv2fe a)

let abs_fe x = Un (Abs, x)
let pow_fe x i = Un (Pow i, x)
let neg_fe x = Un (Neg, x)
let sqrt_fe x = Un (Sqrt, x)
let sqr_fe x = Un (Sqr, x)
let exp_fe x = Un (Exp, x)
let log_fe x = Un (Log, x)

let sin_fe x = Un (Sin, x)
let cos_fe x = Un (Cos, x)
let tan_fe x = Un (Tan, x)
let cot_fe x = Un (Cot, x)

let asin_fe x = Un (Asin, x)
let acos_fe x = Un (Acos, x)
let atan_fe x = Un (Atan, x)
let acot_fe x = Un (Acot, x)

let (=.~) = equal
let (<=.~) = le
let (>=.~) x y = le y x

let bc3_eq x y = bc3_equal (x -.~ y)
let bc3_is_le_cstr x y = bc3_is_le_cstr (x -.~ y)
let bc3_is_le x y = bc3_is_le (x -.~ y)

let bc4_eq x y = bc4_equal (x -.~ y)
let bc4_le x y = bc4_le (x -.~ y)
let bc4_is_le_cstr x y = bc4_is_le_cstr (x -.~ y)
let bc4_is_le x y = bc4_is_le (x -.~ y)

let is_le_cstr = is_le_cstr
let is_le = is_le
let (<=.~~) x y = Var_int (is_le x y)
let (>=.~~) x y = Var_int (is_le y x)

let eval = eval_exp

let fe2fv e =
  let se = simplify e in
  match se with
    Cst x -> Fcl_float_var.FloatVar.elt x
  | Var x -> x
  | _ ->
      let v = Fcl_float_var.FloatVar.create (eval se) in
      Fcl_cstr.post (Var v =.~ se);
      v

let get_vars = get_var_list
