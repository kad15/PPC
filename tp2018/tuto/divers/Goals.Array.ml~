open Facile
open Easy

(* 
minimise x**2 + y**2
subject to x + y = 10
*)

let enum_couples =
  Goals.forto 1 3
    (fun i ->
      Goals.forto 4 5
        (fun j ->
          Goals.atomic (fun () -> Printf.printf "%d-%d\n" i j))) in
Goals.solve enum_couples;;
