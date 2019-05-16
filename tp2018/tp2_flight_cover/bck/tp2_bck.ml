open Facile
open Easy

let read = fun file ->
  let chin = open_in file in
  let (m, n) = Scanf.fscanf chin " %d %d " (fun m n -> (m, n)) in
  let flights = Array.make_matrix m n 0 in
  let costs = Array.make n 0 in
  for j = 0 to n-1 do
    let (cj, nb1) =
      Scanf.fscanf chin " %d %d %d " (fun cj nb1 _ -> (cj, nb1)) in
    costs.(j) <- cj;
    for k = 0 to nb1-1 do
      let i = Scanf.fscanf chin " %d " (fun i -> i) in
      flights.(i-1).(j) <- 1
    done
  done;
  close_in chin;
  (costs, flights)
   



let indomain_max =
  Goals.instantiate (fun d -> Domain.max d)

let solve = fun costs flights ->
  let n = Array.length costs in
  let vars = Fd.array n 0 1 in

  Array.iter
    (fun fi -> Cstr.post (Arith.scalprod_fd fi vars =~ i2e 1))
    flights;

  let cost = Arith.e2fd (Arith.scalprod_fd costs vars) in

  (*let labeling = Goals.Array.forall Goals.indomain vars in*)
  let labeling = Goals.Array.forall indomain_max vars in

  let start = Sys.time () in
  let solution = fun c ->
    let duration = Sys.time () -. start in
    (*Printf.printf "\n%d in %gs: %a\n" c duration Fd.fprint_array vars in*)
    Printf.printf "\n%d in %gs\n" c duration in
  let goal = Goals.minimize labeling cost solution in
  
  let control = fun bt -> Printf.printf "\r%d bt%!" bt in
  ignore (Goals.solve ~control goal);
  Printf.printf "\nProof in %gs\n" (Sys.time () -. start)

let () =
  (*
    let costs = [|5;1;2;1;4;1;3;2;1;2|] in
    let flights =
      [|[|1;0;1;0;1;0;0;0;0;1|];
        [|1;1;1;1;1;1;0;1;0;0|];
        [|1;0;0;1;0;1;1;1;1;0|];
        [|1;1;0;0;0;0;1;0;1;1|]|] in
   *)
    let file = Sys.argv.(1) in
    let (costs, flights) = read file in
    solve costs flights
