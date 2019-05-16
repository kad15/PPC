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
let transpose matrix  =
  let nb_lig = Array.length matrix and nb_col = Array.length matrix.(0) in
  Array.init nb_col (fun j -> Array.init nb_lig (fun i -> matrix.(i).(j)))
    
(* print array *)
let print_array a =
  Array.iter (fun x -> Printf.printf "%d " x) a

(* print array of array *)
let print_matrix m =
  let print_array a =
  Array.iter (fun x -> Printf.printf "%d " x) a in
   Array.iter (fun mi -> print_array mi;print_newline()) m

(* mat
1 2 3
4 5 6
*)
let () = 
  let mat = [|[|1;2;3|];[|4;5;6|]|] in
  print_matrix mat;
  let tmat = transpose mat  in
 print_matrix tmat;
    
