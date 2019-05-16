(*
ocamlopt -I lib unix.cmxa graphics.cmxa maze.cmx tp2.ml

ocamlc -o tp2.out unix.cma graphics.cma maze.cmo tp2.ml
ocamlopt -o tp2.opt unix.cmxa graphics.cmxa maze.cmx tp2.ml
*)

let speed = (0. /. 10000.)

let solve maze =
(* solve_rec prend 2 entiers définissant la pos de la cellule*)
  let rec solve_rec x y = Maze.sleep speed; 
    match Maze.get maze x y with
(*Ainsi, on obtient les conditions d’arrêt de l’algorithme récursif :
— si la cellule à considérer est marquée comme :
— occupée par un mur (Wall),
— déjà explorée (Marked),
— ou sans issue (Dead),
on renverra false ;*)
      Maze.Wall | Maze.Marked | Maze.Dead -> false
(*si la cellule correspond à une sortie (Exit), on marquera la cellule comme faisant partie
du chemin solution (Path) et on renverra true.*)
    | Maze.Exit -> Maze.path maze x y; true
(*Sinon, c’est-à-dire si la cellule est libre (Free), on la marque comme étant explorée (Marked)
et on essaie récursivement les quatre directions possibles. Si l’une de ces tentatives renvoie true,
on marque la cellule comme faisant partie de la solution (Path) et on renvoie true ; sinon (i.e.
toutes les directions renvoient false), on marque la cellule comme sans issue (Dead) et on renvoie
false.*)
    | Maze.Free -> Maze.mark maze x y;
(* si une des cellules est voisine est de type exit on marque la cell x y à Path *)
	if solve_rec (x-1) y || solve_rec (x+1) y || 
           solve_rec x (y-1) || solve_rec x (y+1) then 
           ( 
             Maze.path maze x y; 
             true 
           )
	else 
        ( 
          Maze.dead maze x y; 
          false
        ) 
    | Maze.Path -> failwith "unreachable" in
(* renvoie les coord du point de départ *)
  let (sx, sy) = Maze.start maze in
(* on execute solve_rec avec les conditions intiales sx sy*)
  solve_rec sx sy


(*programme principal *)
let () =
(* lecture du fichier texte maze1.txt passé en argument*)
  let maze = Maze.read Sys.argv.(1) in
(*  *)
  Maze.init maze; (* initalise la fenetre graphique *)
  if solve maze then Printf.printf "Maze solved\n"
  else Printf.printf "No exit\n";
  Maze.wait_key () 


