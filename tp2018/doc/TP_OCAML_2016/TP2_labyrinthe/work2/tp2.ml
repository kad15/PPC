module M = Maze

(*type cell = Free | Wall | Exit | Marked | Dead | Path *)
(* Type of cells. *)
(* depl vers l'ouest (x-1,y) *)
(* depl vers l'est (x+1,y) *)
(* depl vers le sud (x,y-1) *)
(* depl vers l'ouest (x,y+1) *)


let speed = 0.001

let  solve maze =    (*renvoie true si solution, false sinon*)
    let rec aux x y = 
       Maze.sleep speed;
       match M.get maze x y with
       | M.Wall | M.Marked | M.Dead -> false
       | M.Exit -> M.path maze x y;true
       | M.Free -> M.mark maze x y;
	  if aux (x-1) y || aux (x + 1) y ||
             aux x (y - 1) || aux x (y + 1) 
          then ( M.path maze x y ; true ) 
          else ( M.dead maze x y; false)
       | M.Path -> failwith "unreachable" in
   let (x0,y0) = M.start maze in
   aux x0 y0 ;;



  


let () =
  let maze = M.read Sys.argv.(1) in
    M.init maze;
    if (solve maze) then print_endline " solution found !\n"
    else print_endline "no solution !\n";
    Maze.wait_key ();;

