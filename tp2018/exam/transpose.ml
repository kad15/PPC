(*
let transpose matrix m n =
  let matrix_t = Array.make_matrix n m 0 in
  for i = 0 to m-1 do
    for j= 0 to n-1 do
      matrix_t.(i).(j) <-  matrix.(j).(i)
    done
  done; matrix_t
*)
(*  let m = Array.init n (fun i -> Array.sub flat (i*n) n) in
  let tm = Array.init n (fun j -> Array.init n (fun i -> m.(i).(j)))   
*)
    
let () = let col = 3 and lig = 2 in
  let mat = [|[|1;2;3|];[|4;5;6|]|] in
  let tmat = Array.init col (fun j -> Array.init lig (fun i -> mat.(i).(j))) in
  Printf.printf "%d" mat.(0).(1)
    
