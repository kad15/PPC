
type 'a state = 'a

type 'a user_fun = {
  do_at_extraction:
  (float, 'a) Pqueue.t -> 'a Memory.t -> 'a state -> unit;
  do_at_insertion: 'a state -> 'a state -> unit;
}

(*-------------------*)
(* The A* algorithm  *)

let search user_fun u0 is_goal next k h =
  (* Initialize the priority queue and the memory table *)
  let cost0= 0. in
  let f0= cost0 +. h u0 in
  let m= Memory.init u0 cost0 in
  let q= ref (Pqueue.insert f0 u0 Pqueue.empty) in
  (* !!!!!!   PARTIE A MODIFIER !!!!!! *)
  []
  (* !!!!!!   FIN DE LA PARTIE A MODIFIER !!!!!! *)

