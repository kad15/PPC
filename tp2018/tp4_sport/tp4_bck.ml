open Facile open Easy

let fprint_sol ch sol =
  Array.iteri
    (fun i roundi ->
      Printf.fprintf ch "round %d: " i;
      for j=0 to (Array.length roundi) /2 -1 do
        Printf.fprintf ch "(%2d,%2d)"
          (Fd.elt_value roundi.(2*j))  (Fd.elt_value roudi.(2*j+1))
      done;
      Printf.fprintf ch "\n" ) sol

let solve e q l d =
  let eq = e * q in
  let round = Array.init l (fun _ -> Fd.array (2*d) 0 (eq -1)) in

  Array.iter
    (fun roundi ->
      (* ne joue qu'une fois pour chaque round*)
      Cstr.post(Alldiff.cstr roundi);
      (* universités différente pour chaque match et symétries sur les matchs *)
      for j = 0 to d-1 do
        Cstr.post
          ((fd1e roundi.(2*j) /~ i2e q) <~ (fd2e roundi.(2*j+1) /~ i2e q))
      done) round;

(* pas deux fois le meme sport *)
  let dummy = Fd.elt 0 in
  for j = 0 to d-1 do
    let sportj = Array.make (2*l) dummy in
    for i = 0 to l-1 do
      sportj.(2*i) <- round.(i).(2*j);
      sportj.(2*i+1) <- round.(i).(2*j+1)
    done;
    Cstr.post (Alldiff.cstr sportj)
  done;

  (* pas deux fois le même match *)
  let matches = Array.make (l*d) dummy in
  for i = 0 to l-1 do
    for j = 0 to d-1 do
      matches.(i*d+j) <-
        Arith.e2fd (fd2e round.(i).(2*j) *~ i2e eq +~ fd2e round.(i).(2*j+1))
    done
  done;
  Cstr.post (Alldiff.cstr matches);

  (* symmetry on rounds *)
  for i = 0 to l-2 do
    Cstr.post (fd2e round.(i).(0) <~ fd2e round.(i+1).(0))
  done;

  let flat = Array.concat (Array.to_list round) in
  let select =
    Goals.Aray.choose_index (fun v1 v2 -> Fd.size v1 < Fd.size v2) in
  let goal = Goals.Array.forall ~select Goals.indomain flat in

  let control = fun bt -> Printf.printf "\r%d%!" bt in
  let start = Sys.time () in
  if Goals.solve ~control goal then
    Printf.printf "\nSolution found in %gs\n%a\n"
      (Sys.time () -. start) frpitn_sol round
  else Printf.printf "\n No solution\n"


  
  

let () =
 let e = 3 and q=4 and l=6 and d=6 in
   solve e q l d
