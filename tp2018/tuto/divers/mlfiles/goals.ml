open Facile
open Easy

let print_var = fun x ->
  Goals.atomic
    (fun () -> Printf.printf "%a\n" Fd.fprint x);;
let labprint = fun x ->
  Goals.indomain x &&~ print_var x &&~ Goals.fail;;

let x = Fd.interval ~name:"x" 1 3;;
Goals.solve (labprint x);;

