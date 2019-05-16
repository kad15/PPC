open Facile
open Easy



(*le mode standard des buts de labeling étiquetage  instancie non deterministeùment une seule variable en essayant chaque  valeur disjonctivement val1 ou val2 ou val3 etc encore dans le domaine de cet var et de façon croissante *)
(*Pur être exécuté un goal doit être passé en argument de la focntion Goals.solve qui retourne true si le goal réussit.

  illustration*)
let gprint_fd x = Goals.atomic (fun () -> Printf.printf "%a\n" Fd.fprint x)


 let x = Fd.create (Domain.create [-4;2;12]);;
Goals.solve((Goals.indomain x &&~ gprint_fd x &&~ Goals.fail) ||~ Goals.success);;

let label_and_print labeling v =
  (labeling v &&~ gprint_fd v &&~ Goals.fail) ||~ Goals.success;;

Goals.solve (label_and_print (Goals.instantiate Domain.max) x);;




