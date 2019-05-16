open Facile
open Easy
open Scanf
open Printf

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



    

(*
  printf "%d %d %d\n" n m nbc;
  for i = 0 to nbc-1 do
   printf "%d %d %d\n" cc.(i).(0) cc.(i).(1) cc.(i).(2)
  done;
  for i = 0 to n-1 do
   printf "%d " genders.(i)
  done
*)

    
(*
let () =
  let file = Sys.argv.(1) in
  read file
    *)
(*    
let () =
  let file = Sys.argv.(1) in
  let (m,n,cc,genders) = read file in
  printf "%d" m
*)  
  


    

    

(*
1. [2pt] Define the boolean (0/1) decision variables matrix
 table where table.(i).(j) will be
instantiated to 1 if guest j is seated at table i and to 0 otherwise.
 *)

    

let solve = fun (m,n,nbc,cc,genders) ->
  let n = 20 and m = 4 in
  let table = Array.init m (fun _ -> Fd.array n 0 1) in


 
 

(*    
2. [4pt] Add the main structural constraints of the problem:
• the number of guests at each table is k;
• a guest is seated at exactly one table.
 *)


(* cstr k guests per table*)
  let k = (n / m) in
  (* pour chaque element i de de la matrice table i.e. pour chaque ligne car c'est un tableau de tableau *)
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

(*4. [2pt] Add the compatibility constraints using the auxiliary variables previously defined.*)
(* cstr compatibility *)
(*for i = 0 to nbc-1 do*)
Array.iter (fun cci ->

Cstr.post
(
(  (i2e cci.(2) =~ i2e 0) =>~~ (  fd2e guests.(cci.(0)) <>~ fd2e guests.(cci.(1))  )  ) ||~~
((i2e cci.(2) =~ i2e 1) =>~~ (fd2e guests.(cci.(0)) =~ fd2e guests.(cci.(1))))
)

) cc

(*5. [3pt] Add a labeling goal that assigns the boolean variables table to find a solution to this
  problem with a carefully chosen value ordering to perform an efficient search. Justify your choice.*)

  


    (*

let () =
    let file = Sys.argv.(1) in
    let (costs, flights) = read file in
    solve costs flights

*)

    



    




      




      
(*
TRANSPOSEE : let () = let col = 3 and lig = 2 in
  let mat = [|[|1;2;3|];[|4;5;6|]|] in
  let tmat = Array.init col (fun j -> Array.init lig (fun i -> mat.(i).(j))) in
  Printf.printf "%d" mat.(0).(1)


1 - INIT : val init : int -> (int -> 'a) -> 'a array
Array.init n f returns a fresh array of length n, with element number i initialized to the
result of f i. In other terms, Array.init n f tabulates the results of f applied to the
integers 0 to n-1.


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
a.(0); f a.(1); ...; f a.(Array.length a - 1); ().

val iteri : (int -> 'a -> unit) -> 'a array -> unit
Same as Array.iter[26.2], but the function is applied with the index of the element as first
argument, and the element itself as second argument.
 
************************************************************
************************************************************
*********************** tp5 ********************************
************************************************************
************************************************************

let solve = fun file ->
  let chin = open_in file in
  let (n, nbtech, maxload) =
    Scanf.fscanf chin " %d %d %d "
      (fun n nbtech maxload -> (n, nbtech, maxload)) in
  let dummy = Fd.elt 0 in
  let t = Array.make n dummy in
  let d = Array.make n 0 in
  let q = Array.make n 0 in
  for i = 0 to n-1 do
    Scanf.fscanf chin " %d %d %d %d "
      (fun di qi lb ub ->
	d.(i) <- di;
	q.(i) <- qi;
	t.(i) <- Fd.interval lb (ub - di))
  done;  
  let trans =
    Array.init n
      (fun i ->
	Array.init n
	  (fun j -> Scanf.fscanf chin " %d " (fun trij -> trij))) in
  close_in chin;
  
  let tech = Fd.array n 0 (nbtech-1) in
  for i = 0 to n-1 do
    for j = i+1 to n-1 do
      Cstr.post
	((fd2e tech.(i) =~ fd2e tech.(j)) =>~~ 
            ((fd2e t.(i) +~ i2e (d.(i) + trans.(i).(j)) <=~ fd2e t.(j)) ||~~ 
             (fd2e t.(j) +~ i2e (d.(j) + trans.(i).(j)) <=~ fd2e t.(i))))
    done
  done;

  let load =
    Array.init nbtech
      (fun i ->
	let bi = Array.map (fun techj -> fd2e techj =~~ i2e i) tech in
	Arith.e2fd (Arith.scalprod q bi)) in
  Array.iter (fun loadi -> Cstr.post (fd2e loadi <=~ i2e maxload)) load;
 
  let ends = Array.mapi (fun i ti -> Arith.e2fd (fd2e ti +~ i2e d.(i))) t in

  let cost = FdArray.max ends in

  let select =
    Goals.Array.choose_index (fun v1 v2 -> Fd.size v1 < Fd.size v2) in
  let labeling =
    Goals.Array.forall ~select Goals.indomain tech &&~
    Goals.Array.forall ~select Goals.indomain t in

  let start = Sys.time () in
  let solution = fun c ->
    let duration = Sys.time () -. start in
    Printf.printf "\ncost %d in %gs:\n" c duration;
    Array.iteri
      (fun i ti ->
	Printf.printf "task %d done by %d: %d --> %d\n"
	  i (Fd.elt_value tech.(i)) (Fd.elt_value ti) (Fd.elt_value ends.(i)))
      t;
    Printf.printf "load: ";
    Array.iteri
      (fun i loadi -> Printf.printf "tech %d:%d " i (Fd.elt_value loadi))
      load;
    Printf.printf "\n" in

  let goal = Goals.minimize labeling cost solution in

  let control = fun bt -> Printf.printf "\r%d bt%!" bt in
  ignore (Goals.solve ~control goal);
  Printf.printf "\nProof in %gs\n" (Sys.time () -. start)

let () =
  let file = Sys.argv.(1) in
  solve file























************************************************************
************************************************************
*********************** tp4 ********************************
************************************************************
************************************************************

let fprint_sol = fun ch sol ->
  Array.iteri
    (fun i roundi ->
      Printf.fprintf ch "round %d: "i;
      for j = 0 to (Array.length roundi) / 2 - 1 do
	Printf.fprintf ch "(%2d,%2d)"
	  (Fd.elt_value roundi.(2*j)) (Fd.elt_value roundi.(2*j+1))
      done;
      Printf.fprintf ch "\n")
    sol

let solve = fun e q l d ->
  let eq = e * q in
  let round = Array.init l (fun _ -> Fd.array (2*d) 0 (eq-1)) in

  Array.iter
    (fun roundi ->
      (* play once during each round *)
      Cstr.post (Alldiff.cstr roundi);
      (* different universities for each match + symmetry on matches *)
      for j = 0 to d-1 do
	Cstr.post
	  ((fd2e roundi.(2*j) /~ i2e q) <~ (fd2e roundi.(2*j+1) /~ i2e q))
      done)
    round;

  (* not twice the same sport *)
  let dummy = Fd.elt 0 in
  for j = 0 to d-1 do
    let sportj = Array.make (2*l) dummy in
    for i = 0 to l-1 do
      sportj.(2*i) <- round.(i).(2*j);
      sportj.(2*i+1) <- round.(i).(2*j+1)
    done;
    Cstr.post (Alldiff.cstr sportj)
  done;

  (* not twice the same match *)
  let matches = Array.make (l*d) dummy in
  for i = 0 to l-1 do
    for j = 0 to d-1 do
      matches.(i*d+j) <-
	Arith.e2fd (fd2e round.(i).(2*j) *~ i2e eq +~ fd2e round.(i).(2*j+1))
    done
  done;
  Cstr.post (Alldiff.cstr matches);

  (* symmetry on rounds *)
  for i = 0 to l-2 do
    Cstr.post (fd2e round.(i).(0) <~ fd2e round.(i+1).(0))
  done;

  (* balance the opponents universities for each team *)
  if l mod (e-1) = 0 && q >= l / (e-1)  then
    begin
      let d2 = 2*d in
      let teamvsskl = Array.make (l*d2) dummy in
      for i = 0 to l-1 do
	for j = 0 to d-1 do
	teamvsskl.(i*d2 + 2*j) <-
	  Arith.e2fd
	    (fd2e round.(i).(2*j) *~ i2e e +~ fd2e round.(i).(2*j+1) /~ i2e q);
	teamvsskl.(i*d2 + 2*j+1) <-
	  Arith.e2fd
	    (fd2e round.(i).(2*j+1) *~ i2e e +~ fd2e round.(i).(2*j) /~ i2e q)
	done
      done;
      let values = Array.make (eq * (e-1)) 0 in
      let k = ref 0 in
      for i = 0 to eq-1 do
	for j = 0 to e-1 do
	if i / q <> j then
	  begin
	    values.(!k) <- i * e + j;
	    incr k
	  end
	done
      done;
      let nb_each = Fd.elt (l / (e-1)) in
      let cardvals = Array.map (fun v -> (nb_each, v)) values in
      let level = Gcc.Medium in
      Cstr.post (Gcc.cstr ~level teamvsskl cardvals)
    end;

  let flat = Array.concat (Array.to_list round) in
  let select =
    Goals.Array.choose_index (fun v1 v2 -> Fd.size v1 < Fd.size v2) in
  let goal = Goals.Array.forall ~select Goals.indomain flat in

  let control = fun bt -> Printf.printf "\r%d%!" bt in
  let start = Sys.time () in
  if Goals.solve ~control goal then
    Printf.printf "\nSolution found in %gs\n%a\n"
      (Sys.time () -. start) fprint_sol round
  else Printf.printf "\nNo solution\n"

let () =
  let e = 3 and q = 4 and l = 6 and d = 6 in
  solve e q l d































************************************************************
************************************************************
*********************** tp3 ********************************
************************************************************
************************************************************
let data =
  [|(3,[|2;1;1;1;1;1|]);
    (19,[|10;9;7;6;4;4;3;3;3;3;3;2;2;2;1;1;1;1;1;1|]);
    (112,[|50;42;37;35;33;29;27;25;24;19;18;17;16;15;11;9;8;7;6;4;2|]);
    (175,[|81;64;56;55;51;43;39;38;35;33;31;30;29;20;18;16;14;9;8;5;4;3;2;1|]);
  |]

(* more precise timing with Unix module *)
let time = Unix.gettimeofday

(* with reifications *)
let redundant_reify = fun size t vars i ->
  let inter =
    Array.mapi
      (fun j tj ->
        let ct =
          (fd2e vars.(j) <=~ i2e i) &&~~ (i2e i <~ fd2e vars.(j) +~ i2e tj) in
        Reify.boolean ct)
      t in
  Cstr.post (Arith.scalprod_fd t inter =~ i2e size)

(* with Interval constraints *)
let redundant = fun size t vars i ->
  let inter =
    Array.mapi (fun j tj -> Interval.is_member vars.(j) (i-tj+1) i) t in
  Cstr.post (Arith.scalprod_fd t inter =~ i2e size)
  
let solve = fun size t ->
  let vars = fun () -> Array.map (fun ti -> Fd.interval 0 (size - ti)) t in
  let x = vars () in
  let y = vars () in

  let n = Array.length t in
  for i = 0 to n-1 do
    for j = i+1 to n-1 do
      Cstr.post (* non-overlapping constraint *)
        ((fd2e x.(i) +~ i2e t.(i) <=~ fd2e x.(j)) ||~~
        (fd2e x.(j) +~ i2e t.(j) <=~ fd2e x.(i)) ||~~
        (fd2e y.(i) +~ i2e t.(i) <=~ fd2e y.(j)) ||~~
        (fd2e y.(j) +~ i2e t.(j) <=~ fd2e y.(i)))
    done
  done;

  for i = 0 to size-1 do (* redundant constraints *)
    redundant size t x i;
    redundant size t y i
  done;

  (* permutation symmetry among square of same size *)
  for i = 0 to n-2 do
    if t.(i) = t.(i+1) then
      Cstr.post ((fd2e x.(i) <~ fd2e x.(i+1)) ||~~
                   ((fd2e x.(i) =~ fd2e x.(i+1)) &&~~
                      (fd2e y.(i) <~ fd2e y.(i+1))))
  done;

  (* symmetry along vertical and horizontal axis (and 1st diagonal) *)
  Cstr.post (fd2e x.(0) <=~ i2e ((size - t.(0)) / 2));

  (* symmetry along 2nd diagonal*)
  let bl_idx = Fd.interval 0 (n - 1) in (* index of bottom left square *)
  let zero = Fd.elt 0 in
  Cstr.post (FdArray.get_cstr x bl_idx zero);
  Cstr.post (FdArray.get_cstr y bl_idx zero);
  let tfd = Array.map Fd.elt t in
  let tbl = FdArray.get tfd bl_idx in (* size of bottom left square *)
  let bl2x_idx = Fd.interval 0 (n - 1) in (* idx of 2nd bottom left on x *)
  Cstr.post (FdArray.get_cstr x bl2x_idx tbl);
  Cstr.post (FdArray.get_cstr y bl2x_idx zero);
  let tbl2x = FdArray.get tfd bl2x_idx in (* corresponding size *)
  let bl2y_idx = Fd.interval 0 (n - 1) in (* idx of 2nd bottom left on y *)
  Cstr.post (FdArray.get_cstr x bl2y_idx zero);
  Cstr.post (FdArray.get_cstr y bl2y_idx tbl);
  let tbl2y = FdArray.get tfd bl2y_idx in (* corresponding size *)
  Cstr.post (fd2e tbl2x >=~ fd2e tbl2y);

  let select = (* search strategy *)
    Goals.Array.choose_index (fun v1 v2 -> Fd.min v1 < Fd.min v2) in
  let label = fun vars -> Goals.Array.forall ~select Goals.assign vars in
  let goal = label x &&~ label y in

  let nb = ref 0 in
  let increment = (* count all solutions then succeed *)
    Goals.atomic (fun () -> incr nb; Printf.printf "\nsol #%d\n%!" !nb) in
  let goal = (goal &&~ increment &&~ Goals.fail) ||~ Goals.success in
  
  let fprint = fun file -> (* printing for GNUplot *)
    let ch = open_out file in
    Array.iteri
      (fun i ti ->
        let xi = Fd.elt_value x.(i) and yi = Fd.elt_value y.(i) in
        Printf.fprintf ch "%d %d\n%d %d\n%d %d\n%d %d\n%d %d\n\n"
          xi yi (xi+ti) yi (xi+ti) (yi+ti) xi (yi+ti) xi yi)
      t;
    close_out ch in

  (* print number of backtracks *)
  let control = fun bt -> Printf.printf "\r%d bt%!" bt in
  let start = time () in
  if Goals.solve ~control goal then
    begin
      (*Printf.printf "\nSolution found in %gs\n" (time () -. start);
      fprint "tiled.txt"*)
      Printf.printf "\n%d solution(s) found in %gs\n" !nb (time () -. start);
    end
  else Printf.printf "\nNo solution found\n"

let () =
  let nb = int_of_string Sys.argv.(1) in  (* nb = 0 à 3 pour sélectionner les problèmes *)
  let (size, t) = data.(nb) in
  solve size t





























************************************************************
************************************************************
*********************** tp2 ********************************
************************************************************
************************************************************






let read = fun file ->
  let chin = open_in file in
  let (m, n) = Scanf.fscanf chin " %d %d " (fun m n -> (m, n)) in
  let flights = Array.make_matrix m n 0 in
  let costs = Array.make n 0 in
  for j = 0 to n-1 do
    let (cj, nb1) =
      Scanf.fscanf chin " %d %d %d " (fun cj nb1 _ -> (cj, nb1)) in
    costs.(j) <- cj;
    for k = 0 to nb1-1 do
      let i = Scanf.fscanf chin " %d " (fun i -> i) in
      flights.(i-1).(j) <- 1
    done
  done;
  (costs, flights)

let indomain_max =
  Goals.instantiate (fun d -> Domain.max d)

let solve = fun costs flights ->
  let n = Array.length costs in
  let vars = Fd.array n 0 1 in

  Array.iter
    (fun fi -> Cstr.post (Arith.scalprod_fd fi vars =~ i2e 1))
    flights;

  let cost = Arith.e2fd (Arith.scalprod_fd costs vars) in

  (*let labeling = Goals.Array.forall Goals.indomain vars in*)
  let labeling = Goals.Array.forall indomain_max vars in

  let start = Sys.time () in
  let solution = fun c ->
    let duration = Sys.time () -. start in
    (*Printf.printf "\n%d in %gs: %a\n" c duration Fd.fprint_array vars in*)
    Printf.printf "\n%d in %gs\n" c duration in
  let goal = Goals.minimize labeling cost solution in
  
  let control = fun bt -> Printf.printf "\r%d bt%!" bt in
  ignore (Goals.solve ~control goal);
  Printf.printf "\nProof in %gs\n" (Sys.time () -. start)

let () =
  (*
    let costs = [|5;1;2;1;4;1;3;2;1;2|] in
    let flights =
      [|[|1;0;1;0;1;0;0;0;0;1|];
        [|1;1;1;1;1;1;0;1;0;0|];
        [|1;0;0;1;0;1;1;1;1;0|];
        [|1;1;0;0;0;0;1;0;1;1|]|] in
   *)
    let file = Sys.argv.(1) in
    let (costs, flights) = read file in
    solve costs flights












































************************************************************
************************************************************
*********************** tp1 ********************************
************************************************************
************************************************************

let print_sol = fun ch sol ->
  Array.iter
    (fun soli ->
      Array.iter
        (fun solij ->
          Printf.fprintf ch "%2d " (Fd.elt_value solij))
        soli;
      Printf.fprintf ch "\n")
    sol

let magic = fun n ->
  (* Variables *)
  let n2 = n * n in
  let flat = Fd.array n2 1 n2 in
  let m = Array.init n (fun i -> Array.sub flat (i*n) n) in
  let tm = Array.init n (fun j -> Array.init n (fun i -> m.(i).(j))) in

  (* Constraints *)
  let algo = Alldiff.Bin_matching Fd.on_refine in
  Cstr.post (Alldiff.cstr ~algo flat);
  let diag1 = Array.init n (fun i -> m.(i).(i)) in
  let sum = Arith.sum_fd diag1 in
  let diag2 = Array.init n (fun i -> m.(i).(n-1-i)) in
  Cstr.post (Arith.sum_fd diag2 =~ sum);
  Array.iter (fun mi -> Cstr.post (Arith.sum_fd mi =~ sum)) m;
  Array.iter (fun tmj -> Cstr.post (Arith.sum_fd tmj =~ sum)) tm;

  (* Goals *)
  let print_bt = fun bt -> Printf.printf "\r%d%!" bt in
  let goal = Goals.Array.labeling flat in
  let start = Sys.time () in
  if Goals.solve ~control:print_bt goal then
    let duration = Sys.time () -. start in
    Printf.printf "\nSolution found in %gs:\n%a\n" duration print_sol m
  else Printf.printf "No solution found"

let mindom =
  Goals.Array.choose_index (fun v1 v2 -> Fd.size v1 < Fd.size v2)

let allmagic = fun n ->
  (* Variables *)
  let n2 = n * n in
  let flat = Fd.array n2 1 n2 in
  let m = Array.init n (fun i -> Array.sub flat (i*n) n) in
  let tm = Array.init n (fun j -> Array.init n (fun i -> m.(i).(j))) in

  (* Constraints *)
  let algo = Alldiff.Bin_matching Fd.on_refine in
  Cstr.post (Alldiff.cstr ~algo flat);
  let sum = (n * (n2+1)) / 2 in
  let diag1 = Array.init n (fun i -> m.(i).(i)) in
  Cstr.post (Arith.sum_fd diag1 =~ i2e sum);
  let diag2 = Array.init n (fun i -> m.(i).(n-1-i)) in
  Cstr.post (Arith.sum_fd diag2 =~ i2e sum);
  Array.iter (fun mi -> Cstr.post (Arith.sum_fd mi =~ i2e sum)) m;
  Array.iter (fun tmj -> Cstr.post (Arith.sum_fd tmj =~ i2e sum)) tm;

  (* Symmetry breaking *)
  Cstr.post (fd2e m.(0).(0) <~ fd2e m.(n-1).(n-1));
  Cstr.post (fd2e m.(0).(0) <~ fd2e m.(0).(n-1));
  Cstr.post (fd2e m.(0).(0) <~ fd2e m.(n-1).(0));
  Cstr.post (fd2e m.(n-1).(0) <~ fd2e m.(0).(n-1));

  (* Goals *)
  let nb = ref 0 in
  let goal_print =
    let start = Sys.time () in
    Goals.atomic
      (fun () -> 
        incr nb;
        let duration = Sys.time () -. start in
        Printf.printf "\nSolution found in %gs:\n%a\n"
          duration print_sol m) in
  let labeling = (*Goals.Array.labeling flat in*)
    Goals.Array.forall ~select:mindom Goals.indomain flat in
  let goal = labeling &&~ goal_print &&~ Goals.fail in
  let print_bt = fun bt -> Printf.printf "\r%d%!" bt in
  ignore (Goals.solve ~control:print_bt goal);
  Printf.printf "\n%d solution(s) found\n" !nb

let () =
  let n = int_of_string Sys.argv.(1) in 
  allmagic n

*)
