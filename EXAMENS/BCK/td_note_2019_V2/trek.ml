open Facile open Easy

(* Type of problems *)
type pb = {
    c: int;               (* side length *)
    p: (int * int) array; (* (x, y) points *)
    file: string          (* data file name *)
  }


(* [read file] reads data in [file] and returns a problem of type [pb]. *)
let read = fun file ->
  let chin = open_in file in
  let (n, c) = Scanf.fscanf chin "%d %d " (fun n c -> (n, c)) in
  let p = Array.make n (0, 0) in
  for i = 0 to n-1 do
    p.(i) <- Scanf.fscanf chin "%d %d " (fun x y -> (x, y))
  done;
  close_in chin;
  {c; p; file}

(* [fprint_sol file pb x y] prints a solution to [pb] in [file] with
   [x] and [y] the assigned Fd variable arrays corresponding to the
   position of the squares. *)
let fprint_sol = fun file pb x y ->
  let chout = open_out file in
  Array.iter (fun (pxi, pyi) -> Printf.fprintf chout "%d %d\n" pxi pyi) pb.p;
  Printf.fprintf chout "\n";
  Array.iteri
    (fun i xi ->
      let xi = Fd.elt_value xi in
      let xic = xi + pb.c in
      let yi = Fd.elt_value y.(i) in
      let yic = yi + pb.c in
      Printf.fprintf chout "%d %d\n%d %d\n%d %d\n%d %d\n%d %d\n\n"
        xi yi xic yi xic yic xi yic xi yi)
    x;
  close_out chout

(* [extreme_coordinates points] returns the quadruplet  (xmin, xmax, ymin, ymax)
   corresponding to the extremal coordinates contained in array [points]. *)
let extreme_coordinates = fun points ->
  Array.fold_right
    (fun (x, y) (xmin, xmax, ymin, ymax) ->
      (min xmin x, max xmax x, min ymin y, max ymax y))
    points
     (max_int, min_int, max_int, min_int) 
   (* (min_int, max_int, min_int, max_int) *)


(* transpose matrix *)
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

(*
1 - le nombre maximum de carrés est obtenu pour un path rectiligne avec 
des points séparés par leur distance
max c. donc si on a n points, on aura (n-1) carrés i.e. (n-1) 
est la borne sup du nombre de carrés nécessaires
pour couvrir le path
*)

(*
2 - 
 *)
let solve = fun pb ->
  
(*Array.iteri (fun i (x,y)->Printf.printf "{index %d, Position =(%d, %d) }\n"i x y) pb.p;*)
  let (xmin, xmax, ymin, ymax) = extreme_coordinates pb.p in
  let n = Array.length pb.p in
  let q = n - 1 in (* max number of squares = n-1 *)
  let z = pb.c+1 in (* z pour ajuster les domaines cf. question 7°*)
  let xs = Fd.array q (xmin-z)(xmax+z) in (* decision vars are xs, ys which represent 
the lower left point of the square of size c *) 
  let ys = Fd.array q (ymin-z) (ymax+z) in

  (* 3- compute auxiliary vars bij *)
  (* read x and y coordinates of tuple (x,y) in array pb.p  *)
  let px = Array.map (fun (x,y) -> x) pb.p in
  let py = Array.map (fun (x,y) -> y) pb.p in
  (* le point i doit verifier xi dans [x_carre, x_carre+c] idem pour yi *)
  let fd_reified_cstr = fun i j -> Reify.boolean( (i2e px.(i) >~ fd2e xs.(j)) &&~~
                                       (fd2e xs.(j)<~ i2e (px.(i) + pb.c)) &&~~
                                       (i2e py.(i) >~ fd2e ys.(j)) &&~~
                                       (fd2e ys.(j) <~ i2e (py.(i)+pb.c))) in
  (* creation of a matrix of Fd variables  *)
  let b = Array.init n (fun _ -> Fd.array q 0 1) in
  for i = 0 to n-1 do
    for j = 0 to q-1 do
      b.(i).(j) <-fd_reified_cstr i j
    done
  done;

(* let b= Array.mapi (fun i bi -> Array.mapi (fun j bij -> fd_reified_cstr i j) bi) b in*)

(* 4 - [pi, pi+1] in Sj iff bij=1 and bi+1,j=1  *)
(* seg est une matrice carré q x q avec q = n-1 *)
let seg = Array.init q (fun _ -> Fd.array q 0 1) in
   for i = 0 to q-1 do
    for j = 0 to q-1 do
      seg.(i).(j) <- Reify.boolean((fd2e b.(i).(j)   =~ i2e 1) &&~~
                                   (fd2e b.(i+1).(j) =~ i2e 1))  
    done
   done;


(* 5 - Constraints : chaque ligne = segment de la matrice 
des segments doit contenir au moins un 1 indiquant qu'un segment au moins
appartient à un carré j donné
*)
Array.iter (fun segi -> Cstr.post( Arith.sum_fd segi >=~ i2e 1)) seg;

(* 6- Goals : solution search*)
let label_array = Goals.Array.labeling in
let label_matrix = Goals.Array.forall label_array in
    



(* 7 - minimisation du nombre de carrés *)
(* transposee de la matrice b : tb a q lignes ie q carrés.
sur chaque ligne si la somme est >=1  le carre est utlisé 
*)
let tb = transpose b  in
(*let used = Fd.array q 0 1 in*)      
let used = Array.map (fun ci -> Reify.boolean(Arith.sum_fd ci >=~ i2e 1)) tb in
let cost =Arith.e2fd ( Arith.sum_fd used) in
let labeling =  label_matrix seg &&~
  label_array xs   &&~
  label_array ys &&~
  label_array used in

let solution = fun c -> Printf.printf "\n%d" c  in
  let goal = Goals.minimize labeling cost solution in

(* 8 - solving *)

(* 9 - Breaking the permutation symetry among hte squares *)

(* 10 - post the constraints to place all unused squares at the same location (just far enough to ensure that no point is covered using reifications*)

if Goals.solve goal then
  fprint_sol  "sol.txt" pb xs ys
else
  Printf.printf "no solutions" 

    
let () =
    let pb = read "trek1.txt" in
    solve pb 
      
