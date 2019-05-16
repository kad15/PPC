open Facile open Easy

let data =
  [|(3,[|2;1;1;1;1;1|]);
    (19,[|10;9;7;6;4;4;3;3;3;3;3;2;2;2;1;1;1;1;1;1|]);
    (112,[|50;42;37;35;33;29;27;25;24;19;18;17;16;15;11;9;8;7;6;4;2|]);
    (175,[|81;64;56;55;51;43;39;38;35;33;31;30;29;20;18;16;14;9;8;5;4;3;2;1|]);
  |]

let time = Unix.gettimeofday

(* reification*)
let redundant_reify = fun size t vars i ->
  let inter  =
    Array.mapi (fun j tj ->
      let ct =
        (fd2e vars.(j) <=~ i2e i) &&~~ (i2e i <~ fd2e vars.(j) +~ i2e tj) in
      Reify.boolean ct) t in
  Cstr.post(Arith.scalprod_fd t inter =~ i2e size)

(* with interval constraints *)
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
      Cstr.post (* cstr de non recouvrement *)
        ((fd2e x.(i) +~ i2e t.(i) <=~ fd2e x.(j)) ||~~
        (fd2e x.(j) +~ i2e t.(j) <=~ fd2e x.(i)) ||~~
        (fd2e y.(i) +~ i2e t.(i) <=~ fd2e y.(j)) ||~~
        (fd2e y.(j) +~ i2e t.(j) <=~ fd2e y.(i)))
    done
  done;


  for i = 0 to size-1 do (* cstr redondantes*)
    redundant size t x i;
    redundant size t y i
  done;


  (* symetrie  de permutation entre carrés de même taille *)
  for i=0 to n-2 do
    if t.(i) = t.(i-1) then
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


  let select = (* strategie de recherche *)
    Goals.Array.choose_index (fun v1 v2 -> Fd.min v1 < Fd.min v2) in
  let label = fun vars -> Goals.Array.forall ~select Goals.assign vars in
  let goal = label x &&~ label y in

  let nb = ref 0 in
  let increment = (* compte toutes les solutions puis réussit*)
    Goals.atomic (fun () -> incr nb; Printf.printf"\nsol #%d\n%!" !nb) in
  let goal = (goal &&~ increment &&~ Goals.fail) ||~ Goals.success in

  let fprint = fun file -> (* print pour gnu plot*)
    let ch = open_out file in
    Array.iteri (fun i ti ->
      let xi = Fd.elt_value x.(i) and yi = Fd.elt_value y.(i) in
      Printf.fprintf ch "%d %d\n%d %d\n%d %d\n%d %d\n%d %d\n\n"
        xi yi (xi+ti) yi (xi+ti) (yi+ti) xi (yi+ti) xi yi)
      t;
    close_out ch in

  (*  impression du nombre de backtracks *)
  let control = fun bt -> Printf.printf "\r%d bt%!" bt in  (*%! flush the output,  \r carriage return (CR) *)
  let start = time () in
  if Goals.solve ~control goal then
    begin
      Printf.printf "\n%d solution(s) found in %gs\n" !nb (time() -. start);
    end
  else Printf.printf "\n No solution found\n"


let () =
  let nb=int_of_string Sys.argv.(1) in
  let (size, t) = data.(nb) in
  solve size t
      
