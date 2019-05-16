open Facile
open Easy

let print_sol = fun ch sol ->
  Array.iter
    (fun soli ->
      Array.iter
        (fun solij ->
          Printf.fprintf ch "%2d " (Fd.elt_value solij))
        soli;
      Printf.fprintf ch "\n")
    sol

let magic = fun n ->
  (* Variables *)
  let n2 = n * n in
  let flat = Fd.array n2 1 n2 in
  let m = Array.init n (fun i -> Array.sub flat (i*n) n) in
  let tm = Array.init n (fun j -> Array.init n (fun i -> m.(i).(j))) in

  (* Constraints *)
  let algo = Alldiff.Bin_matching Fd.on_refine in
  Cstr.post (Alldiff.cstr ~algo flat);
  let diag1 = Array.init n (fun i -> m.(i).(i)) in
  let sum = Arith.sum_fd diag1 in
  let diag2 = Array.init n (fun i -> m.(i).(n-1-i)) in
  Cstr.post (Arith.sum_fd diag2 =~ sum);
  Array.iter (fun mi -> Cstr.post (Arith.sum_fd mi =~ sum)) m;
  Array.iter (fun tmj -> Cstr.post (Arith.sum_fd tmj =~ sum)) tm;

  (* Goals *)
  let print_bt = fun bt -> Printf.printf "\r%d%!" bt in
  let goal = Goals.Array.labeling flat in
  let start = Sys.time () in
  if Goals.solve ~control:print_bt goal then
    let duration = Sys.time () -. start in
    Printf.printf "\nSolution found in %gs:\n%a\n" duration print_sol m
  else Printf.printf "No solution found"

let mindom =
  Goals.Array.choose_index (fun v1 v2 -> Fd.size v1 < Fd.size v2)

let allmagic = fun n ->
  (* Variables *)
  let n2 = n * n in
  let flat = Fd.array n2 1 n2 in
  let m = Array.init n (fun i -> Array.sub flat (i*n) n) in
  let tm = Array.init n (fun j -> Array.init n (fun i -> m.(i).(j))) in

  (* Constraints *)
  let algo = Alldiff.Bin_matching Fd.on_refine in
  Cstr.post (Alldiff.cstr ~algo flat);
  let sum = (n * (n2+1)) / 2 in
  let diag1 = Array.init n (fun i -> m.(i).(i)) in
  Cstr.post (Arith.sum_fd diag1 =~ i2e sum);
  let diag2 = Array.init n (fun i -> m.(i).(n-1-i)) in
  Cstr.post (Arith.sum_fd diag2 =~ i2e sum);
  Array.iter (fun mi -> Cstr.post (Arith.sum_fd mi =~ i2e sum)) m;
  Array.iter (fun tmj -> Cstr.post (Arith.sum_fd tmj =~ i2e sum)) tm;

  (* Symmetry breaking *)
  Cstr.post (fd2e m.(0).(0) <~ fd2e m.(n-1).(n-1));
  Cstr.post (fd2e m.(0).(0) <~ fd2e m.(0).(n-1));
  Cstr.post (fd2e m.(0).(0) <~ fd2e m.(n-1).(0));
  Cstr.post (fd2e m.(n-1).(0) <~ fd2e m.(0).(n-1));

  (* Goals *)
  let nb = ref 0 in
  let goal_print =
    let start = Sys.time () in
    Goals.atomic
      (fun () -> 
        incr nb;
        let duration = Sys.time () -. start in
        Printf.printf "\nSolution found in %gs:\n%a\n"
          duration print_sol m) in
  let labeling = (*Goals.Array.labeling flat in*)
    Goals.Array.forall ~select:mindom Goals.indomain flat in
  let goal = labeling &&~ goal_print &&~ Goals.fail in
  let print_bt = fun bt -> Printf.printf "\r%d%!" bt in
  ignore (Goals.solve ~control:print_bt goal);
  Printf.printf "\n%d solution(s) found\n" !nb

let () =
  let n = int_of_string Sys.argv.(1) in 
  allmagic n

