open Facile
open Easy


let () = let vs = Fd.array 3 0 10 in
Cstr.post(
  Arith.sum
  ((* au moins 2 variables du tableau  sont >5 *)
   Array.map (fun v -> fd2e v >~~ i2e 5) vs) >=~ i2e 2);
Cstr.post(fd2e vs.(1)<=~ i2e 5);

