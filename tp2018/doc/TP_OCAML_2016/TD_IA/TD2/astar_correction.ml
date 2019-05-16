type 'a state = 'a

type 'a user_fun = {
  do_at_extraction:
  (float, 'a) Pqueue.t -> 'a Memory.t -> 'a state -> unit;
  do_at_insertion: 'a state -> 'a state -> unit;
}

(*-------------------*)
(* The A* algorithm  *)


let search user_fun u0 is_goal next k h =
  (* Version avec une boucle récursive *)
  let cost0= 0. in
  let f0= cost0 +. h u0 in
  let m= Memory.init u0 cost0 in
  let rec loop q =
    try
      let (_prio,u,q)= Pqueue.extract q in
      user_fun.do_at_extraction q m u;
      if is_goal u then (* Reconstruire le chemin complet *)
	Memory.get_path m u
      else (* Calculer les noeuds fils, evaluer, et inserer dans la file *)
	if not (Memory.already_expanded m u) then (
	  Memory.tag_as_expanded m u;
	  let offspring= next u in
	  let cu= Memory.get_cost m u in
	  let new_q=
	    List.fold_left
	      (fun q v ->
		let c= cu +. k u v in
		if not (Memory.mem m v) 
		  || Memory.get_cost m v > c then
		  begin
		    let e= c +. h v in
		    Memory.store_state m v c u;
		    user_fun.do_at_insertion u v;
		    Pqueue.insert e v q
		  end
		else q)
	      q offspring in
	  loop new_q )
	else loop q
    with Pqueue.Empty -> failwith "Pas de solution" in
  let q= Pqueue.insert f0 u0 Pqueue.empty in
  loop q


(* La même chose, dans le style impératif *)

exception Eureka

let search user_fun u0 is_goal next k h =
  (* Initialisation *)
  let cost0= 0. in
  let f0= cost0 +. h u0 in
  let m= Memory.init u0 cost0 in
  let q= ref (Pqueue.insert f0 u0 Pqueue.empty) in
  let path = ref [] in
  try
    while not (Pqueue.is_empty !q) do
      let (_prio,u,new_q)= Pqueue.extract !q in
      q:= new_q;
      user_fun.do_at_extraction !q m u;
      if is_goal u then (* Reconstruire le chemin complet, puis sortir *)
	begin 
	  path:= Memory.get_path m u;
	  raise Eureka
	end;
      if not (Memory.already_expanded m u) then (
	Memory.tag_as_expanded m u;
	let offspring= next u in
	let cu= Memory.get_cost m u in
	List.iter
	  (fun v ->
	    let c= cu +. k u v in
	    if not (Memory.mem m v)
	      || Memory.get_cost m v > c then
	      begin
		let e= c +. h v in
		Memory.store_state m v c u;
		user_fun.do_at_insertion u v;
(*	      if not (Pqueue.mem v !q) then --> perte de temps *)
		let d= Memory.get_depth m v in
		q:= Pqueue.insert e (*float d*) (* -.c+. h v*) v !q
	      end)
	  offspring)
    done;
    failwith "Unreachable"
  with Eureka -> !path



