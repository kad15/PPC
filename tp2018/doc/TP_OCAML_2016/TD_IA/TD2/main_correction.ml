
open Problem
open Draw

let manhattan= ref false

(* Avec déplacements horizontaux et verticaux uniquement *)

let manhattan_dist (x1,y1) (x2,y2) =
  let dx= abs (x2-x1) and dy=abs (y2-y1) in
  float (dx+dy)

let check_constraints pb l =
  List.filter
    (fun (x,y) ->
      x>=pb.xmin && x<=pb.xmax && y>=pb.ymin && y<=pb.ymax&& pb.grid.(x).(y))
    l

let next1 pb (x,y)= 
  let l= [x,y-1;x,y+1;x-1,y;x+1,y] in
  check_constraints pb l

(* Avec déplacements horizontaux, verticaux, ou le long des diagonales *)

let euclidian_dist (x1,y1) (x2,y2) =
  let dx= float (x2-x1) and dy=float (y2-y1) in
  sqrt (dx*.dx+.dy*.dy)

let next2 pb (x,y)= 
  let l= [x,y-1;x,y+1;x+1,y-1;x+1,y;x+1,y+1;x-1,y-1;x-1,y;x-1,y+1] in
  check_constraints pb l


(*-----------------*)
(* Program options *)

let slowmotion= ref false
let sleeptime= ref 0.05
let display= ref true

let options= [
  ("-s",Arg.Set slowmotion, "slow motion");
  ("-t",Arg.Set_float sleeptime, "time between slow motion steps");
  ("-nd",Arg.Clear display, "no graphic display");
  ("-m",Arg.Set manhattan, "use Manhattan distance. Horizontal and vertical moves only")
]

let anonymous_fun= fun s -> failwith (Printf.sprintf "There should be no anonymous arguments: %s" s)

let usage_msg= "./test [options]"

(*--------------------*)
(* Main test function *)

let events = ref [Graphics.Button_down;Graphics.Key_pressed;Graphics.Poll]

let main () =
  (* On parse la ligne de commande *)
  Arg.parse options anonymous_fun usage_msg;

  (* Initialisation du problème et de la fenêtre graphique *)
  let pb= init_problem () in
  let trace= chars_of pb in
  print trace;
  flush stdout;
  let graph= if !display then Draw.open_graph pb else {Draw.height=0;width=0} in
  let scale= scale graph pb in
  if !display then Draw.display scale pb.orig [] [] [] pb;

  (* Définition des arguments à passer à [Astar.search] *)
  let dist, next=
    if !manhattan then (manhattan_dist, next1 pb)
    else (euclidian_dist, next2 pb) in
  let u0= pb.orig in
  let is_goal u = u=pb.dest in
(**)  let h (x,y)=  dist (x,y) pb.dest in (**)
(**  let h (x,y) = 0. in **)
  let k u v= dist u v in
  let do_at_extraction =
    fun q memory (x,y) -> 
      let path= Memory.get_path memory (x,y) in
      let frontier= Pqueue.elements q in
      if !display then (
	let closed= Memory.closed_list memory in
	Draw.display scale (x,y) path frontier closed pb;
	Draw.wait !events);
      if !slowmotion then Draw.sleep !sleeptime ;
      trace.(x).(y)<- 'd' in
  let do_at_insertion = fun _u (x,y) -> trace.(x).(y)<- 'g' in

  (* Recherche du chemin le plus court et affichage *)
  try
    let user_fun=
      { Astar.do_at_extraction= do_at_extraction;
	Astar.do_at_insertion = do_at_insertion } in
    let path= Astar.search user_fun u0 is_goal next k h in
    List.iter (fun (x,y) -> trace.(x).(y)<- ' ') path;
    print trace;flush stdout;
    if !display then (
      events := [Graphics.Button_down;Graphics.Key_pressed];
      Draw.wait !events;
      Graphics.close_graph () )
  with Exit ->
    if !display then Graphics.close_graph ();;


(*---------------------------------*)
(* Appel de la fonction principale *)

main ()
