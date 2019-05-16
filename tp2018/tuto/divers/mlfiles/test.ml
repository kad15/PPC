open Facile
open Easy

let t1 = Array.init 10  (fun i ->i+1);;
let t2 = Fd.array 10 1 10;;

let ps = Arith.e2fd (Arith.scalprod_fd t1 t2);;
let ps2 = Arith.scalprod_fd t1 t2;;


Printf.printf " ps=%a \n" Fd.fprint ps;;

let [|x;y;z|] as vars = Fd.array 3 0 10;;
(*Fd.fprint_array stdout vars;;*)

let ineq =
  fd2e x *~ fd2e y -~ i2e 2 *~ fd2e z >=~ i2e 90;;

Cstr.fprint stdout  ineq;;
Cstr.post ineq;;
Cstr.fprint stdout ineq;;

Fd.fprint_array stdout vars;;







(*
let vars = Fd.array 4 0 9;;
Fd.fprint_array stdout vars;

Fd.is_var vars.(0);;

Printf.printf " %d \n" (Fd.size vars.(2))

*)


(*Fd.elt_value vars.(3);;*)
  
(*
let x = Fd.interval ~name:"varX" (-42) 1729;;
(*let x = Fd.interval (-42) 1729;;*)

(*Fd.fprint stdout x*)
Printf.printf "x=%a\n" Fd.fprintf x;;

let dom = Domain.create [1;2;3;5;7;8;20;21];;

let y = Fd.create dom;;
Fd.fprint stdout y

(*Printf.printf "x=%a\n" Fd.fprintf x*)
Printf.printf "xxx = %d" (Fd.elt_value x) 
*)
