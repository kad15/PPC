(*
module Hello :  
sig 
  val hello : unit -> unit
end = 
struct
  let message = "Hello"
  let hello () = print_endline message
end
*)

(* rédaction plus élégante : on voit qu'on défini a part d'abord la signature de hello
sous forme d'un module type puis par'*)
module type Hello_type =
sig
  val hello : unit -> unit
end
  
module Hello : Hello_type =
struct
  ...
end  

(* At this point, Hello.message is not accessible anymore. *)
(* car non déclaré dans la partie signature sig du sous module Hello*)
let goodbye () = print_endline "Goodbye"
(*)let toto = Hello.message  donne Error: Unbound value Hello.message*)
let hello_goodbye () =
  Hello.hello ();
  goodbye ()
