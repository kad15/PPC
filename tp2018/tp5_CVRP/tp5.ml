open Facile
open Easy

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
    Goals.Array.forall ~select:select Goals.indomain tech &&~
    Goals.Array.forall ~select:select Goals.indomain t in

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
  ignore (Goals.solve ~control:control goal);
  Printf.printf "\nProof in %gs\n" (Sys.time () -. start)

let () =
  let file = Sys.argv.(1) in
  solve file
