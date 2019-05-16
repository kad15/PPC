let s0 = 10
let c = 1.125


let make () = ref(Array.make (s0+1) 0)


(*
3. Écrire la fonction d’accès au i e élément d’un tableau extensible :
val get : int array ref -> int -> int
en utilisant la fonction :
val invalid_arg : string -> ’a
pour lever une exception si on essaie d’accéder à un élément non encore utilisé.
*)

(* le 1er élement contient le nombre d'élements du tableau référencé par a *)
(* les data sont rangées de 1 à n et on doit simuler doit un tableau d'indice de 0 à n-1*)
(* j'ai un tab a de taille 11 qui contient 10 data  aux indices de 1 à 10 avec a.(0) qui contient 10 *)
(* dans un tableau classique accéder au i eme elt, c renvoyer t.(i-1)*)
(* on veut simuler un tableau donc le premier elt a le rang 0 et ds le tableau t il a pour rang 1 *)
let get a i = if i < !a.(0) then !a.(i+1)
              else invalid_arg "index out of bounds"

let set a i = if i < !a.(0) then !a.(i+1)
              else invalid_arg "index out of bounds"

let set a i x = if i < !a.(0) then !a.(i+1) <- x
                 else invalid_arg "index out of bounds"

let append a x =  
  let t = !a in
  let n = Array.length t -1 in
  if t.(0) < n then
  (
    t.(t.(0)+1) <- x;
    t.(0) <- t.(0) + 1 
  )
  else
  (
    let s = truncate(c *. float n) +1 in
    let et = Array.make s 0 in
    for i=0 to n do et.(i)<- t.(i) done;
    et.(0)<- et.(0)+1;
    et.(n+1)<- x;
    a := et
  )

let () = 
  let fic = 
  open_out "fic_time" in
  output_string fic "toto\n";
  close_out fic;
  print_endline "Done!\n"  

let time n k file = 
  let ts = Array.init k (fun _ -> make()) in 
  let chout = open_out file in
  let s = ref 0. in
  for i=1 to n do
    let start = Sys.time() in
    for j=0 to k-1 do append ts.(j) i done;
    let duration = Sys.time() -. start in
    s := !s +. duration;
    Printf.fprintf chout "%d %f %f\n" i duration (!s /. float i) done;
    close_out chout
    



let time n k file =
  let ts = Array.init k (fun _ -> make ()) in
  let chout = open_out file in
  let s = ref 0. in
  for i = 1 to n do
    let start = Sys.time () in
    for j = 0 to k-1 do
      append ts.(j) i done;
    let duration = Sys.time () -. start in
    s := !s +. duration;
    Printf.fprintf chout "%d %f %f\n" i duration (!s /. float i) done;
  close_out chout









