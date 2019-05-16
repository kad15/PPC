open Facile
open Easy
  (* tab xs de n variables de domaine 0 à n-1*)
let n = 10
let xs = Fd.array n 0 (n-1)
(* contrainte réifiée x == entier i   *)
let is_equal_to i x = (fd2e x =~~ i2e i);;
Array.iteri
    (fun i xi ->
      let cardi = Arith.sum (Array.map (is_equal_to i) xs) in
      Cstr.post (fd2e xi =~ cardi))
  xs;

Goals.solve (Goals.Array.forall Goals.indomain xs)
