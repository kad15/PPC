open Facile
open Easy

let  solve () =
let [|s;e;n;d;m;o;r;y|] as vars = Fd.array 8 0 9 in

Cstr.post (fd2e s <>~ i2e 0);
Cstr.post (fd2e m <>~ i2e 0);
Cstr.post (Alldiff.cstr vars);

let send =
i2e 1000 *~ fd2e s +~ i2e 100 *~ fd2e e +~ i2e 10 *~ fd2e n
+~ fd2e d in
let more =
i2e 1000 *~ fd2e m +~ i2e 100 *~ fd2e o +~ i2e 10 *~ fd2e r
+~ fd2e e in
let money =
i2e 10000 *~ fd2e m +~ i2e 1000 *~ fd2e o +~ i2e 100 *~ fd2e n
+~ i2e 10 *~ fd2e e +~ fd2e y in
Cstr.post (send +~ more =~ money);

(* search goal *)
let goal = Goals.Array.labeling vars in
(* resolution *)
if Goals.solve goal then
let [|s;e;n;d;m;o;r;y|] = Array.map Fd.elt_value vars in
Printf.printf "%d%d%d%d\n+ %d%d%d%d\n= %d%d%d%d%d\n"
s e n d m o r e m o n e y
else
Printf.printf "No solution found\n"


let () =
 solve ()


 
 






