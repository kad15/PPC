(* lecture du nombre N sur l'entrée standard  *)
Printf.printf "%s" "saisir le nombre max pour le calcul des nombres premiers.\n"
let max = read_int ()

(* création d'un tableau de booléens nommé prime utilise la fonction de bibliothèque Array.make et init des cases à la val par défaut true '*)
let prime = Array.make (max+1) true


let () = 
  prime.(0) <- false
  prime.(1) <- false
  let limit = truncate (sqrt (float max)) in (* limit est la partie entiere de racine carrée de max*)
  for n = 2 to limit do
    if prime.(n) then begin
      let m = ref (n*n) in
      while !m <= max do
        prime.(!m)<-false;
        m := !m +n
      done
    end
  done

let () = 
  for n = 2 to max do
    if prime.(n) then Printf.printf "%d " n
  done
let () =
  Printf.printf "%s\n" "" 
  


