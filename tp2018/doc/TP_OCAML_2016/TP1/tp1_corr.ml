(* size of extensible int arrays as the first element *)
let s0 = 10
let c = 1.125

let make = fun () -> ref (Array.make (s0+1) 0)

let get = fun a i ->
  let t = !a in
  if i < t.(0) then t.(i+1)
  else invalid_arg "index out of bounds"

let set = fun a i x ->
  let t = !a in
  if i < t.(0) then t.(i+1) <- x
  else invalid_arg "index out of bounds"

let append = fun a x ->
  let t = !a in
  let n = Array.length t - 1 in
  if t.(0) < n then begin
    t.(t.(0)+1) <- x;
    t.(0) <- t.(0) + 1 end
  else begin
    let s = truncate (c *. float n) + 1 in
    let et = Array.make s 0 in
    (*** Array.blit t 0 et 0 (n+1); ***)
    for i = 0 to n do et.(i) <- t.(i) done;
    et.(0) <- et.(0) + 1;
    et.(n+1) <- x;
    a := et end

let time n k file =
  let ts = Array.init k (fun _ -> make ()) in
  let chout = open_out file in
  let s = ref 0. in
  for i = 1 to n do
    let start = Sys.time () in
    for j = 0 to k-1 do
      append ts.(j) i done;
    let duration = Sys.time () -. start in
    s := !s +. duration;
    Printf.fprintf chout "%d %f %f\n" i duration (!s /. float i) done;
  close_out chout

let () =
  let n = int_of_string Sys.argv.(1) in
  let k = int_of_string Sys.argv.(2) in
  let file = Sys.argv.(3) in
  time n k file

