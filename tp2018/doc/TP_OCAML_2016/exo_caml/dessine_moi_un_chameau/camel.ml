(* You do not need to modify this file for hw02. *)

;; open Graphics

let run () : unit =
  open_graph "";
  auto_synchronize false;  
  clear_graph ();
  synchronize ();

 
  let y_max = size_y () in

  let orange = rgb 198 141 62 in
  set_color orange;
 
  (* Draw ellipse with top left corner (x,y) and width and height *)
  let fe = fun x y w h ->
    let y = y_max - y in
    fill_ellipse (x + (w / 2)) (y - (h / 2)) (w / 2) (h / 2)
  in

  (* Draw polygon using top left corner as origin *)
  let fp = fun arr ->
    fill_poly (Array.map (fun (x, y) -> (x, y_max - y)) arr)
  in

  fe 185 90 250 147; (* main body *)
  fe 269 54 68 98; (* left hump *)
  fe 143 138 127 94; (* torso *)

  set_color white;
  fe 89 (-79) 195 227; (* clipping *)

  set_color orange;
  fe 134 93 62 122; (* neck *)
  fe 97 101 86 47; (* head *)
  fe 354 63 68 118; (* right hump *)
  fe 367 101 98 109; (* rump *)
  fe 247 176 68 94; (* shoulders *) 

  let front_legs = [|
    (256, 246); (249, 287); (251, 343); (264, 346); (266, 306); (276, 276);
    (282, 306); (278, 343); (292, 343); (298, 306); (298, 277); (299, 254); 
    (255, 246)
  |] in fp front_legs; 
  
  let back_legs = [|
    (432, 243); (441, 289); (430, 334); (445, 334); (462, 275); (469, 328);
    (476, 328); (478, 259); (454, 214); (461, 164); (407, 206)
  |] in fp back_legs;
  
  (* move the cursor and draw the text *)
  moveto 200 40;
  set_color black;
  draw_string "Bactrian the Double-Humped OCaml";

  synchronize ();

  (* loop forever *)
  let rec loop () : unit = 
    let _ = 
      wait_next_event [Mouse_motion; Button_down; Button_up; Key_pressed] in
    loop ()
  in
  loop ()

;; run ()
