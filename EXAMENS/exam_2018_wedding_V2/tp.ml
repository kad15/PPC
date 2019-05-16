open Facile open Easy
open Scanf open Printf

(* ********* outils perso ********** *)

  
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

(* autre version de flatten  *)
let flatten2 mat = Array.concat (Array.to_list mat)
    
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

(* ********* fin outils perso ********** *)


let read = fun file ->
  let chin = open_in file in
  let (n,m) = fscanf chin "%d %d " (fun n m -> (n,m)) in
  let nbc = fscanf chin "%d" (fun nbc -> nbc) in (* nbc : nb de cstr de compatibilité*)
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
  done ; (n,m,nbc,cc,genders) 
     
let print_sol ch sol =
  Array.iteri
    (fun i soli ->
      fprintf ch "table %d : " i;
      Array.iteri
        (fun j solij ->
          if Fd.elt_value solij = 1 then
            fprintf ch "%2d " j) soli;
      fprintf ch "\n") sol

let solve (n, m, nbc, cc, genders)=
(* 1  *)
(* m : nombre de tables = nb de ligne de la matrice table*)
(* n : nb d'invités *)
  let table = Array.init m (fun _ -> Fd.array n 0 1) in

(*  2. [4pt] Add the main structural constraints of the problem: *)
(* • the number of guests at each table is k; *)
let k = n/m in
      Array.iter
      (fun tablei -> Cstr.post(Arith.sum_fd tablei =~ i2e k) )
      table;
(* • a guest is seated at exactly one table. *)
  (* les invités sont les m colonnes de la matrice table *)
  (* la somme sur les colonnes de table doit donc être = 1 *)
  (* <=> somme sur les lignes de la transposée de table = 1  *)
  
let t_table = transpose table in
Array.iter
      (fun t_tablei -> Cstr.post(Arith.sum_fd t_tablei =~ i2e 1) )
     t_table;

(* 3. [3pt] Define auxiliary variables guest 
where guest.(j) represents the table at which guest j is *)
(* seated, according to the following expression defined for any j: *)
let coefs = Array.init m (fun i -> i) in
(* print_array coefs; print_newline (); *)
(*  Array.init m (fun i -> table.(i).(j)) donne la  colonne j du tableau 2D table *)

let ps coefs vect = Arith.e2fd (Arith.scalprod_fd coefs vect) in
let guest =
  Array.init n (fun j -> ps coefs (Array.init m (fun i -> table.(i).(j))  )) in


Fd.fprint_array stdout guest;
printf "\n n =%d\n" n;

(*4. [2pt] Add the compatibility constraints using the auxiliary variables previously defined. *)
for i = 0 to nbc-1 do

    if cc.(i).(2)=0 then
      Cstr.post(fd2e guest.(cc.(i).(0)) <>~ fd2e guest.(cc.(i).(1)));
    if cc.(i).(2)=1 then
       Cstr.post(fd2e guest.(cc.(i).(0)) =~ fd2e guest.(cc.(i).(1)))
done;

(* 5. [3pt] Add a labeling goal that assigns the boolean variables table to find a solution to this *)
(* problem with a carefully chosen value ordering to perform an efficient search. Justify your choice. *)

(* 6. [2pt] Execute the search to provide a solution and print the number of backtracks. *)
  
(* 7. [4pt] Males and females numbers must be balanced as much as possible at each table. Define *)
(* auxiliary variables to represent the discrepancy (i.e. the absolute value of the difference) between *)
(* the number of males and the number of females at each table. Then define the cost as the *)
(* maximum difference for all tables and optimize the solution to the problem. *)
(* Remark: Function Arith.abs: Arith.t -> Arith.t can be used to return the absolute value *)
(* of an arithmetic expression. *)



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
let goal = Goals.minimize label cost solution in
let control= fun bt -> printf "\r%d bt%!" bt in
ignore (Goals.solve ~control goal);
printf "\nProof in %gs\n" (Sys.time () -. start)

    
let () =
  let file = Sys.argv.(1) in
  let (n,m,nbc,cc,genders) = read file in
  solve (n, m,nbc,cc,genders)
      
