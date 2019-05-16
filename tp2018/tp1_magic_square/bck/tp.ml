open Facile
open Easy
open Scanf
open Printf

let print_sol = fun ch sol ->
  Array.iter
    (fun soli ->
      Array.iter
        (fun solij ->
          Printf.fprintf ch "%2d " (Fd.elt_value solij))
        soli;
      Printf.fprintf ch "\n")
    sol

  
(* transpose matrix   *)
let transpose matrix  =
  let nb_lig = Array.length matrix and nb_col = Array.length matrix.(0) in
  Array.init nb_col (fun j -> Array.init nb_lig (fun i -> matrix.(i).(j)))
    
(* print array *)
let print_array a =
  Array.iter (fun x -> Printf.printf "%d " x) a;
    print_newline()

(* print array of array *)
let print_matrix m =
  let print_array a =
  Array.iter (fun x -> Printf.printf "%d " x) a in
  Array.iter (fun mi -> print_array mi;print_newline()) m;
  print_newline()

(* flatten a  matrice n * m in an array of size n*m *)
let flatten mat =
  let n = Array.length mat and m = Array.length mat.(0) in
  let flat = Array.make (n*m) 0 in
  for i = 0 to n-1 do
    for j = 0 to m-1 do
      flat.(m*i+j) <- mat.(i).(j)
    done
  done;
  flat
    
(* transform an ocaml array vector of size n*m in a matrix n*m  *) 
let array_to_matrix vector n m =
  Array.init n (fun i -> Array.sub vector (i*m) m)

(* return the first diagonal of a matrice n*n *)
let diagonal1 mat = let n = Array.length mat in
Array.init n (fun i -> mat.(i).(i))

(* return the second diagonal of a square matrice *)
let diagonal2 mat =
  let n = Array.length mat in
  Array.init n (fun i -> mat.(i).(n-1-i))



 
let magic n =
  (* variables *)
  let n2 = n *n in
  let flat = Fd.array n2 1 n2 in
  let m = array_to_matrix flat n n in
  let tm = transpose m in


  (* Cstr *)
  (* utilisation de l'algorithme bin matching avec l'evenement c =Fd.on_refine
on_refine : event
Event occuring when a variable is changed, i.e. its domain modified.
val on_subst : event
Event occuring when a variable is instantiated.
  *)
  let algo = Alldiff.Bin_matching Fd.on_refine in
  Cstr.post (Alldiff.cstr ~algo flat);
  let diag1 = diagonal1 m in
  let sum = Arith.sum_fd diag1 in
  let diag2 = diagonal2 m in
  Cstr.post(Arith.sum_fd diag2 =~ sum);
  Array.iter (fun mi -> Cstr.post (Arith.sum_fd mi =~ sum)) m;
  Array.iter (fun tmi -> Cstr.post (Arith.sum_fd tmi =~ sum)) tm;


 (* Symmetry breaking *)
 (* Cstr.post (fd2e m.(0).(0) <~ fd2e m.(n-1).(n-1));
  Cstr.post (fd2e m.(0).(0) <~ fd2e m.(0).(n-1));
  Cstr.post (fd2e m.(0).(0) <~ fd2e m.(n-1).(0));
  Cstr.post (fd2e m.(n-1).(0) <~ fd2e m.(0).(n-1));
*)
   
  (* Goals *)
(*
  let mindom_strategy = Goals.Array.choose_index (fun v1 v2 -> Fd.size v1 < Fd.size v2) in
  let labeling = Goals.Array.forall ~select:mindom_strategy Goals.indomain flat in
  
    let nb = ref 0 in
  let goal_print  =
    let start = Sys.time () in
    Goals.atomic (fun () -> incr nb;
      let duration = Sys.time () -. start in
      Printf.printf "\nSolution found in %gs:\n%a\n" duration print_sol m ) in

  let goal = labeling &&~ goal_print &&~ Goals.fail in
  let print_bt bt = printf " \r%d%!" bt in
  ignore(Goals.solve ~control:print_bt goal); (*on ignore le bool retourne par solve car 
le goal echoue tjrs via Goals.fail  *)
    Printf.printf "\n%d solution(s) found\n" !nb 
  
*)  


  let nb = ref 0 in
  let goal_print = 
    let start = Sys.time () in
    Goals.atomic
      (fun () -> 
        incr nb;
        let duration = Sys.time () -. start in
        Printf.printf "\nSolution found in %gs:\n%a\n"
          duration print_sol  m) in
  let mindom = (* min domaine size heuristic strategy. 
"choose index order fds" returns the index of the best (minimun) free (not
instantiated) variable in the array fds for the criterion order. *)
  Goals.Array.choose_index (fun v1 v2 -> Fd.size v1 < Fd.size v2) in
  let labeling = (*Goals.Array.labeling flat in*) (*heuristique select *)
    Goals.Array.forall ~select:mindom Goals.indomain flat in
  let goal = labeling &&~ goal_print &&~ Goals.fail in
  (* print the number of backtracks *)
  let print_bt = fun bt -> Printf.printf " \r%d" bt in
  ignore (Goals.solve ~control:print_bt goal); (*on ignore le bool retourne par solve car 
le goal echoue tjrs via Goals.fail  *)
  Printf.printf "\n%d solution(s) found\n" !nb

    
    
 

let () =
  let n = int_of_string Sys.argv.(1) in 
  magic n


(*        
let () =
  let vec = [|2;7;6;20;9;5;1;20;4;3;8;20 |] in
let  matrice = array_to_matrix vec 3 4 in
(*let m = [| [|2;7;6;20|];[|9;5;1;20|];[|4;3;8;20|] |] in
  let flat = flatten m in *)
  print_matrix matrice
*)


    (*

#let findall g x =
# let sol = ref [] in
# let store = Goals.atomic (fun () -> sol := Fd.elt_value x :: !sol) in
# let goal = g x &&~ store &&~ Goals.fail in
# ignore (Goals.solve goal);
# !sol;;
val findall :
(Facile.Easy.Fd.t -> Facile.Goals.t) ->
Facile.Easy.Fd.t -> Facile.Easy.Fd.elt list = <fun>
We first declare a reference sol on an empty list to store all the solutions.
 Then the simple
goal store is defined to push any new solution on the head of sol – note
 that we here use
Fd.elt_value v (see 4.16) for conciseness but it is quite unsafe
 unless we are sure that v is
bound. The main goal is the conjunction of g, store and a failure.
 This goal obviously always
fails, so we “ignore” the boolean returned by Goals.solve, 
and the solutions list is eventually
returned.

*)
