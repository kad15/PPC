open Facile
open Easy
open Scanf
open Printf



let print_sol = fun ch sol ->
  Array.iteri
    (fun i soli ->
       Printf.fprintf ch "table %d : " i ;
      Array.iteri
        (fun j solij ->
          if (Fd.elt_value solij) = 1 then
          Printf.fprintf ch "%2d " j) soli;
      Printf.fprintf ch "\n") sol

let read = fun file ->
  let chin = open_in file in
  let (m, n) = fscanf chin " %d %d " (fun n m -> (m, n)) in
  let nbc = fscanf chin "%d" (fun nbc -> nbc) in
  let cc = Array.make_matrix nbc 3 0 in
  let genders = Array.make n 0 in
  for i = 0 to nbc-1 do
    for j = 0 to 2 do
    let a = fscanf chin " %d " (fun a -> a) in
    cc.(i).(j)<-a;
    done
  done;
  for i = 0 to n-1 do
    let b = fscanf chin " %d " (fun b -> b) in
    genders.(i)<-b
  done ; (m,n,nbc,cc,genders) 


let solve = fun (m,n,nbc,cc,genders) ->
  let table = Array.init m (fun _ -> Fd.array n 0 1) in
  
(* cstr k guests per table*)
  let k = (n / m) in
(* chaque element tablei de la matrice table sont des lignes donc de type Array *)
 Array.iter (fun tablei -> Cstr.post (Arith.sum_fd tablei =~ i2e k)) table;

(*cstr a guest is seated at exactly one table*)
  let tt(*transposee de table m lignes n colonnes *) =
    Array.init n (fun j -> Array.init m (fun i -> table.(i).(j))) in
  Array.iter (fun ttj -> Cstr.post (Arith.sum_fd ttj =~ i2e 1)) tt; 

(*  colj = Array.init m (fun i -> table.(i).(j)) donne la  colonne j du tableau 2D table *)
let ii = Array.init m (fun i -> i) in
let guests =
  Array.init n (
  fun j -> Arith.e2fd (
    Arith.scalprod_fd ii (
    Array.init m (
    fun i -> table.(i).(j))))) in 

(* cstr compatibility si i j 0 dans wedding.txt les tables des deux invités i et j doivent être différentes, identiques si i j 1 *)
Array.iter (fun cci ->
Cstr.post
(
(  (i2e cci.(2) =~ i2e 0) =>~~ (  fd2e guests.(cci.(0)) <>~ fd2e guests.(cci.(1))  )  ) ||~~
((i2e cci.(2) =~ i2e 1) =>~~ (fd2e guests.(cci.(0)) =~ fd2e guests.(cci.(1))))
)
) cc;

(* On calcule pour chaque table de 0 à m-1 le vecteur des nb de femmes nbfs comme prodscal_fd des lignes de tableij avec le vecteur genders  puis la valeur abs
   de la difference = abs(nbf - nbh) = abs(nbf -(k -nbf)) =abs( 2*nbf-k) que l'on range dans un tdiffs  *)

(* array de nb de femmes par table, diffs = Fd.array des variables auxiliaires*)
let nbfs =
  Array.init m (
  fun i -> Arith.e2fd (
    Arith.scalprod_fd genders (
    Array.init n (
    fun j -> table.(i).(j))))) in
let diffs = Array.init m (fun i -> Arith.abs(i2e 2  *~ fd2e nbfs.(i) -~ i2e k )) in
let cost = Arith.e2fd (Arith.sum diffs) in

let flat = Array.concat (Array.to_list table) in
(* Les variables sont choisies selon leur domaine croissant car il est plus efficace
d'échouer au plus tôt dans l'arbre de recherche.
(Goals.indomain x &&~ Goals.fail) ||~ Goals.success
 *)
let mindom = Goals.Array.choose_index (fun v1 v2 -> Fd.size v1 < Fd.size v2) in
let start = Sys.time () in
let solution = fun c ->
  let duration = Sys.time () -. start in
  printf "\ncost=%d in %gs\n%a\n" c duration print_sol table in

let label = Goals.Array.forall ~select:mindom Goals.indomain flat in
let goal = Goals.minimize label cost  solution in
let control= fun bt -> printf "\r%d bt%!" bt in
ignore (Goals.solve ~control goal);
printf "\nProof in %gs\n" (Sys.time () -. start)

    
    
let () =
  let file = Sys.argv.(1) in
  let (m,n,nbc,cc,genders) = read file in
  solve (m,n,nbc,cc,genders)



    



    




      




      
(*
TRANSPOSEE : let () = let col = 3 and lig = 2 in
  let mat = [|[|1;2;3|];[|4;5;6|]|] in
  let tmat = Array.init col (fun j -> Array.init lig (fun i -> mat.(i).(j))) in
  Printf.printf "%d" mat.(0).(1)


1 - INIT : val init : int -> (int -> 'a) -> 'a array
Array.init n f returns a fresh array of length n, with element number i initialized to the
result of f i. In other terms, Array.init n f tabulates the results of f applied to the
integers 0 to n-1.   [| f 0 ; f 1; ...; f (n-1) |]. ex : Array.init n (fun i -> i+1) 
donne le vecteur 1 2 3 ... n


val make_matrix : int -> int -> 'a -> 'a array array
Array.make_matrix dimx dimy e returns a two-dimensional array (an array of arrays)
with first dimension dimx and second dimension dimy. All the elements of this new matrix
are initially physically equal to e.

2 - MAP :val map : ('a -> 'b) -> 'a array -> 'b array
Array.map f a applies function f to all the elements of a, and builds an array with the
results returned by f: [| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |].

val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
Same as Array.map[26.2], but the function is applied to the index of the element as first
argument, and the element itself as second argument.
Array.mapi (fun i ti -> Arith.e2fd (fd2e ti +~ i2e d.(i))) t

3- val iter : ('a -> unit) -> 'a array -> unit
Array.iter f a applies function f in turn to all the elements of a. It is equivalent to f
a.(0); f a.(1); ...; f a.(Array.length a - 1); (). NE CREE PAS DE TABLEAU

val iteri : (int -> 'a -> unit) -> 'a array -> unit
Same as Array.iter[26.2], but the function is applied with the index of the element as first
argument, and the element itself as second argument.



$$$$$$$$$$$$$$


Goals.Array.forall g [| e1 ; ...;en |] = ( g e1 ) &&~ ... &&~( g en )
and has the following type:
(α → Goals.t) → α array → Goals.t
The labelling of an array of variables is the iteration of the instantiation of
one variable with
let Goals.indomain : 

labeling_array = Goals.Array.forall Goals.indomain ;;


and this function obviously has type
F d.t array → Goals.t .
A matrix is an array of arrays; 
labelling of a matrix simply is a composition with the array iterator, 
which can be elegantly written in OCaml:


 
let labeling_matrix = Goals.Array.forall labeling_array ;;

*)
