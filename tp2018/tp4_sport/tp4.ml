open Facile open Easy

let fprint_sol = fun ch sol ->
  Array.iteri(
  fun i roundi -> Printf.fprintf ch "round %d: " i;
    for j = 0 to (Array.length roundi)/2 -1 do
      Printf.fprintf ch "(%2d,%2d)"
        (Fd.elt_value roundi.(2*j)) (Fd.elt_value roundi.(2*j+1))
    done;
    Printf.fprintf ch "\n"
 ) sol

let solve = fun () ->
  let e = 3 (* 3 universites  *) and q = 4 (* 4 teams  *)
  and d = 6 (*6 sports *) and l = 6 (*6 rounds *) in

  (* joue une fois par tour *)
  let round = Array.init l (fun _ -> Fd.array (2*d) 0 (e*q-1)) in
  (* les matches sont joués par des universités différentes  *)
  Array.iter (
  fun roundi -> Cstr.post(Alldiff.cstr roundi);
      for j = 0 to d-1 do
        Cstr.post
          ((fd2e roundi.(2*j) /~ i2e q) <~ (fd2e roundi.(2*j+1) /~ i2e q))
      done
 ) round;


  (* not twice the same sport *)
  let dummy = Fd.elt 0 in
  for j = 0 to d-1 do
    let sportj = Array.make (2*l) dummy in
    for i = 0 to l-1 do
      sportj.(2*i) <- round.(i).(2*j);
      sportj.(2*i+1) <- round.(i).(2*j+1)
    done;
    Cstr.post (Alldiff.cstr sportj)
  done;

  (* not twice the same match *)
  let matches = Array.make (l*d) dummy in
  let eq = e*q in
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
  

  let flat = Array.concat(Array.to_list round) in
  let select = Goals.Array.choose_index(fun v1 v2 -> Fd.size v1 < Fd.size v2) in
  let goal = Goals.Array.forall ~select Goals.indomain flat in
  let control = fun bt -> Printf.printf "\r%d%!" bt in
  let start = Sys.time() in
  if Goals.solve ~control goal then
    Printf.printf "\nSolution found in %gs\n%a\n"
      (Sys.time () -. start) fprint_sol round
  else Printf.printf "\nNO solution\n"

let () =
  solve ()
