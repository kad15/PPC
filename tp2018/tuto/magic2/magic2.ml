open Facile open Easy

  let solve n =
let xs = Fd.array n 0 (n-1) in
let is_equal_to i x =( fd2e x =~~ i2e i ) in
Array.iteri
(fun i xi ->
  let cardi = Arith.sum (Array.map (is_equal_to i) xs) in
  Cstr.post (fd2e xi =~ cardi))
  xs; 
Goals.solve (Goals.Array.forall Goals.indomain xs)

  let () = Printf.printf "%d" 1
