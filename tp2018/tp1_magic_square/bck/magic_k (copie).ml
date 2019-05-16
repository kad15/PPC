open Facile
open Easy

(*
val iter : ('a -> unit) -> 'a array -> unit

Array.iter f a applies function f in turn to all the elements of a. It is equivalent to f a.(0); f a.(1); ...; f a.(Array.length a - 1); ().

val iteri : (int -> 'a -> unit) -> 'a array -> unit

Same as Array.iter, but the function is applied with the index of the element as first argument, and the element itself as second argument.

val map : ('a -> 'b) -> 'a array -> 'b array

Array.map f a applies function f to all the elements of a, and builds an array with the results returned by f: [| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |].

val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array

Same as Array.map, but the function is applied to the index of the element as first argument, and the element itself as second argument.

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a

Array.fold_left f x a computes f (... (f (f x a.(0)) a.(1)) ...) a.(n-1), where n is the length of the array a.

val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a

Array.fold_right f a x computes f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...)), where n is the length of the array a.

val init : int -> (int -> 'a) -> 'a array

Array.init n f returns a fresh array of length n, with element number i initialized to the result of f i. In other terms, Array.init n f tabulates the results of f applied to the integers 0 to n-1.


*)

let print_sol = fun ch sol ->
  Array.iter
    (fun soli -> 
       Array.iter
         (fun solij -> 
          Printf.fprintf ch "%2d " (Fd.elt_value solij))
       soli;
       Printf.fprintf ch "\n")
      sol
(*
val sub : 'a array -> int -> int -> 'a array

Array.sub a start len returns a fresh array of length len, containing the elements number start to start + len - 1 of array a.
*)

let magic = fun n -> 
(* Variables *)
  let n2 = n * n in
  let flat = Fd.array n2 1 n2 in
  let m = Array.init n (fun i -> Array.sub flat (i*n) n ) in
  let tm = Array.init n (fun j-> Array.init n (fun i -> m.(i).(j))) in

  (* Cstr *)
  let algo = Alldiff.Bin_matching Fd.on_refine in
  Cstr.post (Alldiff.cstr ~algo flat);
  let sum = (n * (n2+1)) / 2 in
  let diag1 = Array.init n (fun i -> m.(i).(i)) in
  Cstr.post (Arith.sum_fd diag1 =~ i2e sum);
  let diag2 = Array.init n (fun i -> m.(i).(n-1-i)) in
  Cstr.post (Arith.sum_fd diag2 =~ i2e sum);
  Array.iter (fun mi -> Cstr.post (Arith.sum_fd mi =~ i2e sum)) m ;
  Array.iter (fun tmj -> Cstr.post (Arith.sum_fd tmj =~ i2e sum)) tm;


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
 (* Vars *)
 let n2 = n*n in
 let flat = Fd.Array n2 1 n2 in
 let m = Array.init n (fun i -> Array.sub falt (i*n) n) in
 let tm = Array.init n (fun j -> Array n (fun i -> m.(i).(j))) in

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
