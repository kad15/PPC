open Facile
open Easy

(* 10 valeurs de domaine 0 à n-1 *)
let () =
 let n = 9 in
let vars = Fd.array (n+1) 0 n in
let card_vals =  Array.mapi (fun i x -> (x, i)) vars in
Cstr.post (Gcc.cstr ~level:Gcc.Medium vars card_vals);
(* redundant constraint *)
let vals = Array.init (n+1) (fun i -> i) in
Cstr.post (Arith.scalprod_fd vals vars =~ i2e (n+1));

Goals.solve (Goals.Array.labeling vals)

