(*
ocamlc -o tp2.out unix.cma graphics.cma maze.cmo tp2.ml
ocamlopt -o tp2.opt unix.cmxa graphics.cmxa maze.cmx tp2.ml
*)

let speed = 0.005

let solve = fun maze ->
  let rec solve_rec = fun x y -> Maze.sleep speed;
    match Maze.get maze x y with
      Maze.Wall | Maze.Marked | Maze.Dead -> false
    | Maze.Exit -> Maze.path maze x y; true
    | Maze.Free -> Maze.mark maze x y;
	if solve_rec (x-1) y || solve_rec (x+1) y || solve_rec x (y-1) ||
	   solve_rec x (y+1) then begin Maze.path maze x y; true end
	else begin Maze.dead maze x y; false end
    | Maze.Path -> failwith "unreachable" in
  let (sx, sy) = Maze.start maze in
  solve_rec sx sy

let () =
  let maze = Maze.read Sys.argv.(1) in
  Maze.init maze;
  if solve maze then Printf.printf "Maze solved\n"
  else Printf.printf "No exit\n";
  Maze.wait_key () 

