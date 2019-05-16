module Hello = (*// attantion mettre une majuscue au module Hello pas hello !!!*)
struct
  let message = "Hello"  (* ateention !!! pas de ; *)
  let hello () = print_endline message
end

(*definition  d'un module *)
module <Nom_module> =
struct
....
end


(*def d'une signature d'un module qui donne uniquement les objets accessibles comme les fichiers mli*)
module type Hello_type =
sig
  val hello : unit -> unit
end


(* utilisation  du module Hello avec la signature Hello_type 
le module Hello n'a accès qu'aux objets ds la signature'*)
(* la signature joue le role du fichier mli*)
module Hello : Hello_type =
struct
  ...
end  
