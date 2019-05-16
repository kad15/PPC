open Graphics

let max_iter = 60 (* nombre maximum d'itérations *)
let f_max_iter = float max_iter (* optim *)

let dedans = black

(* couleur = interpolation linéaire entre le rouge (loin) et le vert (près) *)
let interpolation n =
  let f = float n /. f_max_iter in
  rgb (truncate ((1. -. f) *. 255.)) (truncate (f *. 255.)) 0

let couleur xc yc =
  let rec iter i x y =
    if i = max_iter then
      dedans
    else 
      let x2 = x *. x in
      let y2 = y *. y in
      if x2 +. y2 > 4. then
	interpolation i
      else
	iter (succ i) (x2 -. y2 +. xc) (2. *. x *. y +. yc)
  in
  iter 0 xc yc

let dessine xmin xmax ymin ymax gx gy w h pas =
  let dx = float pas *. (xmax -. xmin) /. float w in
  let dy = float pas *. (ymax -. ymin) /. float h in
  for i = 0 to w/pas - 1 do
    for j = 0 to h/pas - 1 do
      let x = xmin +. float i *. dx in
      let y = ymin +. float j *. dy in
      set_color (couleur x y);
      if pas = 1 then
	plot (gx + i) (gy + j)
      else
	fill_rect (gx + i * pas) (gy + j * pas) pas pas
    done
  done

let larg = 900
let haut = 600
let () = open_graph (Printf.sprintf " %dx%d" larg haut)

let w_xmin = ref 0.0
let w_xmax = ref 0.0
let w_ymin = ref 0.0
let w_ymax = ref 0.0

let taches = Queue.create ()

let reset xmin xmax ymin ymax = 
  clear_graph ();
  Queue.clear taches;
  Queue.add (xmin, xmax, ymin, ymax, 0, 0, larg, haut, 16) taches;
  w_xmin := xmin;
  w_xmax := xmax;
  w_ymin := ymin;
  w_ymax := ymax

let rec scheduler () =
  if not (Queue.is_empty taches) then begin
    let (xmin,xmax,ymin,ymax,gx,gy,w,h,pas) = Queue.pop taches in
    dessine xmin xmax ymin ymax gx gy w h pas;
    if pas > 1 then begin
      let pas' = max 1 (pas / 2) in
      let dx = (xmax -. xmin) /. 2. in
      let dy = (ymax -. ymin) /. 2. in
      Queue.add 
	(xmin, xmin +. dx, ymin, ymin +. dy, gx, gy, w/2, h/2, pas') taches;
      Queue.add 
	(xmin +. dx, xmax, ymin, ymin +. dy, gx + w/2, gy, w/2, h/2, pas') 
	taches;
      Queue.add 
	(xmin, xmin +.dx, ymin +. dy, ymax, gx, gy + h/2, w/2, h/2, pas') 
	taches;
      Queue.add 
	(xmin +. dx, xmax, ymin +. dy, ymax, 
	 gx + w/2, gy + h/2, w/2, h/2, pas') 
	taches;
    end
  end;
  let st = wait_next_event [Poll; Key_pressed; Button_down] in
  if st.keypressed then exit 0;
  if st.button then begin
    let st = wait_next_event [Button_up] in
    let dx = !w_xmax -. !w_xmin in
    let xc = !w_xmin +. (float st.mouse_x /. float larg) *. dx in
    let dy = !w_ymax -. !w_ymin in
    let yc = !w_ymin +. (float st.mouse_y /. float haut) *. dy in
    reset (xc -. dx /. 4.) (xc +. dx /. 4.) (yc -. dy /. 4.) (yc +. dy /. 4.)
  end;
  scheduler ()

let () = 
  reset (-2.0) 1.0 (-1.0) 1.0;
  scheduler ();
  ignore (read_key ())



