open Facile
open Easy
open Scanf
open Printf
  

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





    
let solve costs flights =
let n = Array.length costs in  
  (* variables *)
let xs = Fd.array n 0 1 in (* vecteur de variables *)
  (* cstr *)
Array.iter (fun fi -> Cstr.post( Arith.scalprod_fd fi xs =~ i2e 1)) flights;
let cost = Arith.e2fd (Arith.scalprod_fd costs xs) in


(* 
Module Goals also provides the function Goals.instantiate
 that allows to specify the or-
dering strategy of the labeling. 
Goals.instantiate takes as first argument a function to which
is given the current domain of the variable 
(as single argument) and should return an integer
candidate for instantiation
 *)
(* 3. To improve the efficiency of the search, 
modify the variable instantiation strategy by first
trying value 1 before 0. Justify this choice. *)
let indomain_max =
  Goals.instantiate (fun d -> Domain.max d) in
let labeling = Goals.Array.forall indomain_max xs in
  

(*let labeling = Goals.Array.forall Goals.indomain xs in*)
let start = Sys.time () in
let solution c = let duration = Sys.time () -. start in
printf "\n%d in %gs\n" c duration in

let goal = Goals.minimize labeling cost solution in
let control = fun bt -> printf "\r%d bt%!" bt in
ignore(Goals.solve ~control:control goal);
printf "\n Proof in %gs\n" (Sys.time () -. start)





let () =
  
(*    let costs = [|5;1;2;1;4;1;3;2;1;2|] in
    let flights =
      [|[|1;0;1;0;1;0;0;0;0;1|];
        [|1;1;1;1;1;1;0;1;0;0|];
        [|1;0;0;1;0;1;1;1;1;0|];
        [|1;1;0;0;0;0;1;0;1;1|]|] in
  *) 
    let file = Sys.argv.(1) in
    let (costs, flights) = read file in
    solve costs flights
