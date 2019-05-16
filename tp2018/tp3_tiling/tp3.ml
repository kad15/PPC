open Facile open Easy
open Printf open Scanf

let data =
  [|(3,[|2;1;1;1;1;1|]);
    (19,[|10;9;7;6;4;4;3;3;3;3;3;2;2;2;1;1;1;1;1;1|]);
    (112,[|50;42;37;35;33;29;27;25;24;19;18;17;16;15;11;9;8;7;6;4;2|]);
    (175,[|81;64;56;55;51;43;39;38;35;33;31;30;29;20;18;16;14;9;8;5;4;3;2;1|]);
  |]

let fprint = fun filename t x y -> (* printing for GNUplot *)
  let ch = open_out filename in
  Array.iteri
    (fun i ti ->
      let xi = Fd.elt_value x.(i) and yi = Fd.elt_value y.(i) in
      Printf.fprintf ch "%d %d\n%d %d\n%d %d\n%d %d\n%d %d\n\n"
        xi yi (xi+ti) yi (xi+ti) (yi+ti) xi (yi+ti) xi yi)
    t;
  close_out ch

(* more precise timing with Unix module *)
let time = Unix.gettimeofday

(* with interval constraints *)
let redundant size t vars i =
  let inter = (* is_member x lb ub retourne une var booléenne = 1 si lb<= x <= ub *)
    Array.mapi (fun j tj -> Interval.is_member vars.(j) (i-tj+1) i) t in
  Cstr.post(Arith.scalprod_fd t inter =~ i2e size)
    
(* with reification *)
let redundant_reify size t vars i =
  let inter =
    Array.mapi
      (fun j tj ->
        let ct =
          (fd2e vars.(j) <=~ i2e i)
            &&~~
          (i2e i <~ fd2e vars.(j) +~ i2e tj)  in
        Reify.boolean ct) t in
  Cstr.post(Arith.scalprod_fd t inter =~ i2e size)

(* size :  size of the big square 
     t :  array of size of small squares *)
let solve size t =
  (* function that produces an array of variables in the domain 0 size-ti from array t 
   a square of size 2 in a big square of size 3 had to have coordinates between 0 and 3-2  *)
  let vars () = Array.map ( fun ti -> Fd.interval 0 (size-ti) ) t in
  (* x, y : arrays of fd vars = position of the lower left point of a square *)
  let x = vars () and  y = vars () in (* Fd.fprint_array stdout x;print_newline ();Fd.fprint_array stdout y;print_newline ();*)
    (* non overlaping of the squares cstr pairwise *)
  let n = Array.length t in
  let cstr i j z t = (fd2e z.(i) +~ i2e t.(i) <=~ fd2e z.(j)) ||~~
  (fd2e z.(j) +~ i2e t.(j) <=~ fd2e z.(i)) in
for i = 0 to n-2 do
  for j = i+1 to n-1 do
    (*non overlaping according to x and y printf "\n%d %d\n" i j *)
    Cstr.post( (cstr i j x t) ||~~ (cstr i j y t))
  done
done;

  for i = 0 to size-1 do (* redundant constraints *)
    redundant_reify size t x i;
    redundant_reify size t y i
  done;

(* permutation symmetry among squares of same size *)
  (* on trie lexicographiquement selon x puis selon y si x=y *)
 
  for i = 0 to n-2 do
    if t.(i) = t.(i+1) then
      Cstr.post ( (fd2e x.(i) <~ fd2e x.(i+1)) ||~~ ((fd2e x.(i) =~ fd2e x.(i+1)) &&~~
   (fd2e y.(i) <~ fd2e y.(i+1))) )
  done;

  (* symmetry along vertical and horizontal axis (and 1st diagonal) *)
  (* imposer au centre du plus gros carré donc au carré d'index 0 car donnée triées 
par ordre décroissant à 
être dans le quart inférieur gauche du grand carré de taille size = T   
donc on doit avoir x - t/2 <= size/2 et idem pour y   *)
  Cstr.post (
    (fd2e x.(0) <=~ i2e ((size - t.(0)) / 2)) &&~~
    (fd2e y.(0) <=~ i2e ((size - t.(0)) / 2))
            
            );

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

  
let strategy = (*search strategy *)
  Goals.Array.choose_index(fun v1 v2 -> Fd.min v1 < Fd.min v2) in
let label vars = Goals.Array.forall ~select:strategy Goals.indomain vars in
let goal = label x &&~ label y in


let nb = ref 0 in
let increment = (* count all sol that succeed *)
  Goals.atomic (fun () ->incr nb; Printf.printf "\nsol #%d\n%!" !nb;fprint "tiled.txt" t x y ) in
let goal = (goal &&~ increment &&~ Goals.fail) ||~ Goals.success  in

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
  let arg = int_of_string Sys.argv.(1) in
  let (size, t) = data.(arg) in
  solve size t



    
 
