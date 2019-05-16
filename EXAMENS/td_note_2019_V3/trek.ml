open Facile open Easy
  open Printf

(* ****************************************** *)

  
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

(* ************************************************* *)
    
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
 
(* 1 *)
(*
 Cas le plus défavorable: n points espacés de c donc 
 le nombre maximal de carrés est n-1.
 *)

(* 2 *)


(* let () = *)
(*   let (xmin, xmax, ymin, ymax) = extreme_coordinates [|(0,0);(2,2);(1,2)|] in *)
(*   (\* printf "%d %d" xmin ymin *\)  *)
(*   printf "point min (%d,%d), point max (%d,%d)" xmin ymin xmax ymax *)

let solve pb =
  (* c : taille du carré, pts : ensemble des points *) 
  let c = pb.c and pts = pb.p in
  let n = Array.length pts in
  let q = n-1 in                        (*q : nombre max de carrés*)
  let (xmin, xmax, ymin, ymax) =
    extreme_coordinates pts in
  (* Variables et domaine :                                  *)
  (* vecteur x et y : coordonnées bas/gauche de chaque carré *)
  (* let x = Fd.array (n-1) xmin xmax and  y = Fd.array (n-1) ymin ymax in *)
  let xmax2 = xmax+1 and ymax2 = ymax+1 in
  (* let zero a = if a>=0 then a else 0 in *)
  let xmin2 =  (xmin-c-1) and ymin2 = (ymin-c-1) in 
 let x = Fd.array q xmin2 xmax2 and  y = Fd.array q ymin2 ymax2 in (*agrandissement domaine cf. question 7 *)
(* 3 - bij = 1 si le point  dans le carré sj  *)
(* Il faut que pts.(i) soit dans l'interval xj, xj+c  *)
(* et  idem pour y *)
 
  (* b vecteur de n lignes constituées de vecteur de n-1 colonnes  *)
  (* n : nombre de points, q : nombre de segments *)

  let b = Array.init n ( fun _ -> Fd.array q 0 1 ) in

  for i = 0 to n-1 do
    let (xi,yi) = pts.(i) in
    for j = 0 to q-1 do
      b.(i).(j) <- Reify.boolean (
        (fd2e x.(j) <=~ i2e xi) &&~~  (i2e xi <=~ fd2e x.(j) +~ i2e c ) &&~~
        (fd2e y.(j) <=~ i2e yi) &&~~  (i2e yi <=~ fd2e y.(j) +~ i2e c ))
    done
  done;

(* 4 - segij = 1 si le segment pi,pi+1 est dans sj  *)
  (* le segment i,i+1 est dans le carré sj, si bij = 1 et bi+1,j = 1 *)
  (* seg est une matrice carré de taille q=n-1 avec les lignes *)
  (* qui correspondent aux segments et les colonnes aux carrés *)
  let seg = Array.init q ( fun _ -> Fd.array q 0 1) in
  for j = 0 to q-1 do
    for i = 0 to q-1 do
      seg.(i).(j) <-  Reify.boolean((fd2e b.(i).(j) =~ i2e 1) &&~~
                                    (fd2e b.(i+1).(j) =~ i2e 1))
    done
  done;

(* 5- post des cstr : chaque segment inclus dans au moins un carré  *)
  (* un segment i est dans au moins un carré s'il y a au moins un 1   *)
  (* dans la ligne segi de la matrice seg *)
  Array.iter ( fun segi -> Cstr.post( Arith.sum_fd segi >=~ i2e 1) ) seg;

(* 9 - Cstr sym breaking  *)
 
  (* gain de l'ordre de 80% en temps *)
  for i = 0 to q-2  do
    Cstr.post( (fd2e x.(i) <~ fd2e x.(i+1)) );
  done;


 
(* 6- *)
let label_array = Goals.Array.labeling in
let label_matrix = Goals.Array.forall label_array in
  
let label_seg tab =  label_matrix tab in
(* let dom = Goals.Array.choose_index (fun v1 v2 -> let s1 = Fd.size v1 and s2 = Fd.size v2 in 
   s1 <  s2 || (s1=s2 && Fd.constraints_number v1 > Fd.constraints_number v2)) in  --- ~select:dom *)

let label_vars vars = Goals.Array.forall   Goals.indomain vars in
let labeling = label_seg seg &&~ label_vars x &&~ label_vars y in


(* 7 - *)
let used = Fd.array q 0 1 in            
(*tb : transposee de b ainsi chaque ligne avec uniquement
 des zeros désignera un carré non utilisé *)
let tb = transpose b in
(*  *)
Array.iteri  (fun j tbj -> used.(j) <- Reify.boolean(Arith.sum_fd tbj >~ i2e 0) ) tb ;
let cost = Arith.e2fd (Arith.sum_fd used) in


(* (\* 10  *\) *)
(*  let xout = xmax+1 and yout = ymax+1 in *)
(*  Array.iteri *)
(*    ( *)
(*     fun i ui -> Cstr.post( (fd2e ui =~ i2e 0) =>~~ *)
(*                                     ( *)
(*                                      (fd2e x.(i) =~ i2e xout ) &&~~ (fd2e y.(i) =~ i2e yout ) *)
(*                                     ) *)

(*                                     ) *)
(*    ) used; *)


(* (\*let labeling = Goals.Array.forall Goals.indomain xs in*\) *)


(* let goal = Goals.minimize labeling cost solution in *)
(* let control bt pb x y = printf "\r%d bt%!" bt; *)
(* fprint_sol  "sol.txt" pb x y in *)
(* ignore(Goals.solve ~control:control goal); *)
(* printf "\n Proof in %gs\n" (Sys.time () -. start) *)


let start = Sys.time () in 
  let sol cc = 
    let duration = Sys.time () -. start in
    printf "\n nb of squares used : %d in %gs\n" cc duration;
    fprint_sol  "sol.txt" pb x y in
let goal = Goals.minimize labeling cost sol in

(*  Tous les points sont contenus dans l'enveloppe rectangulaire
définie par (xmin, ymin) point en bas à droite du rectangle et 
(xmax,ymax) point en haut à droite de ce rectangle. pour que des carrés
de taille c puissent ne contenir aucun point, il faut agrandir le domaine
de telle sorte qu'il soit défini par (xmin-c-1,ymin-c-1) et (xmax+1,ymax+1)
 *)



(* 8 - Solve the search goal  *)
(* if Goals.solve (goal ||~ Goals.success) then *)
ignore( Goals.solve goal)
(* Vérification des solutions : *)
(* ./trek.opt trek1.txt && gnuplot ; 
   gnuplot> plot [-1:] [-1:] "sol.txt" w l *)

(* 9 - permutation symmetry breaking *)
(* cf . ci-dessus *)




let () =
  let pb = read  Sys.argv.(1) in
  solve pb                            

    

    
