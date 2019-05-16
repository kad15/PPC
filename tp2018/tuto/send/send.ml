open Facile 
open Easy

let solve = fun () -> 
(* Variables *)
let s = Fd.interval 0 9 and e = Fd.interval 0 9 and
n = Fd.interval 0 9 and d = Fd.interval 0 9 and 
m = Fd.interval 0 9 and o = Fd.interval 0 9 and
r = Fd.interval 0 9 and y = Fd.interval 0 9 in
(* Constraints *)
Cstr.post (fd2e m >~ i2e 0);
Cstr.post (fd2e s >~ i2e 0);
let digits = [|s;e;n;d;m;o;r;y|] in
Cstr.post (Alldiff.cstr digits);
let c = Fd.array 3 0 1  in (* Carry array *)
let one x = fd2e x and ten x = i2e 10 *~ fd2e x in
Cstr.post (            one d +~ one e =~ one y +~ ten c.(0));
Cstr.post (one c.(0) +~ one n +~ one r =~ one e +~ ten c.(1));
Cstr.post (one c.(1) +~ one e +~ one o =~ one n +~ ten c.(2));
Cstr.post (one c.(2) +~ one s +~ one m =~ one o +~ ten m);
(* search goal solving *)
if Goals.solve (Goals.Array.labeling digits) then begin
let value = Fd.elt_value in
Printf.printf " %d%d%d%d\n" (value s) (value e) (value n) (value d);
Printf.printf "+  %d%d%d%d\n" (value m) (value o) (value r) (value e);
Printf.printf "=%d%d%d%d%d\n" (value m) (value o) (value n) (value e) (value y)
end else
prerr_endline "No solution\n"

let () = solve ()
