let dicho = fun t x ->
  let i = ref 0 in
  let j = ref (Array.length t - 1) in
  let idx = ref ~-1 in
  while !idx = -1 && !i <= !j do
    let mid = (!i + !j) / 2 in
    if t.(mid) = x then idx := mid
    else if t.(mid) > x then j := mid - 1
    else i := mid + 1
  done;
  !idx

let interpol = fun t x ->
  let i = ref 0 in
  let j = ref (Array.length t - 1) in
  let idx = ref (-1) in
  while !idx = -1 && !i <= !j && t.(!i) <= x && x <= t.(!j) do
    let mid = !i + ((x - t.(!i)) * (!j - !i)) / (t.(!j) - t.(!i)) in
    if t.(mid) = x then idx := mid
    else if t.(mid) > x then j := mid - 1
    else i := mid + 1
  done;
  !idx

let rand_array n =
  let n10 = 10 * n in
  let t = Array.init n (fun _ -> Random.int n10) in
  Array.sort compare t;
  t

let exp_array n =
  Array.init n (fun i -> truncate (exp (float i)))

let rand_elt t =
  let n = Array.length t in
  t.(Random.int n)

let time () = Unix.gettimeofday ()
  
let duration t xs f =
  let start = time () in
  Array.iter (fun x -> ignore (f t x)) xs;
  time () -. start

let () =
  let nmax = int_of_string Sys.argv.(1) in
  let m = int_of_string Sys.argv.(2) in
  let k = int_of_string Sys.argv.(3)  in
  let step = nmax / k in
  let tabs = Array.init k (fun i -> rand_array ((i+1) * step)) in
  (*let tabs = Array.init k (fun i -> exp_array ((i+1) * step)) in*)
  let xs =
    Array.map (fun t -> Array.init m (fun _ -> rand_elt t)) tabs in
  Array.iteri
    (fun i xsi ->
      let tdicho = duration tabs.(i) xsi dicho in
      let tinter = duration tabs.(i) xsi interpol in
      Printf.printf "%d %g %g\n%!" ((i+1)*step) tdicho tinter)
    xs

