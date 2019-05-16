(* -*-compile-command: "ocamlc graphics.cma -o logo tp15bis.ml"; -*- *)

type expression =
    Cst of int
  | Var of string
  | Div of expression * expression

let ex = Div (Cst 360, Var "ne")

type instruction =
    Repeat of expression * instruction
  | Block of instruction list
  | Right of expression
  | Fwd of expression

let exemple = Repeat (Var "ne",
                      Block [Right ex;
                             Repeat (Var "ne",
                                     Block [Right ex;
                                            Fwd (Var "sz")])])

let rec eval = fun exp ->
  match exp with
    Cst c -> c
  | Var id -> failwith id
  | Div (se1, se2) -> eval se1 / eval se2

let eval = fun env exp ->
  let rec eval_rec = fun e ->
  match e with
    Cst c -> c
  | Var id -> List.assoc id env
  | Div (se1, se2) -> eval_rec se1 / eval_rec se2 in
  eval_rec exp

let pi = acos (-1.)

let deg2rad = fun d -> pi *. float d /. 180.

let play = fun env prog ->
  let heading = ref 0 in
  let rec play_rec = fun instr ->
    match instr with
      Repeat (exp, body) ->
        let n = eval env exp in
        for i = 1 to n do play_rec body done
    | Block seq -> List.iter play_rec seq
    | Right exp ->
        let a = eval env exp in
        heading := !heading - a
    | Fwd exp -> 
        let d = eval env exp in
        let a = deg2rad !heading in
        let dx = truncate (cos a *. float d) in
        let dy = truncate (sin a *. float d) in
        Graphics.rlineto dx dy in
  play_rec prog

let ntimes = fun f init n ->
  let rec ntimes_rec = fun i acc ->
    if i > n then acc else
    ntimes_rec (i + 1) (f acc) in
  ntimes_rec 1 init

let play2 = fun env prog ->
  let rec play_rec = fun instr h ->
    match instr with
      Repeat (exp, body) ->
        let n = eval env exp in
        ntimes (play_rec body) h n
    | Block seq ->
        List.fold_left (fun acc instr -> play_rec instr acc) h seq
    | Right exp ->
        let a = eval env exp in
        h - a
    | Fwd exp -> 
        let d = eval env exp in
        let hrad = deg2rad h in
        let dx = truncate (cos hrad *. float d) in
        let dy = truncate (sin hrad *. float d) in
        Graphics.rlineto dx dy;
        h in
  ignore (play_rec prog 0)

type state = {
    h: int;
    x: int;
    y: int
  }

let play3 = fun env prog ->
  let rec play_rec = fun instr state ->
    match instr with
      Repeat (exp, body) ->
        let n = eval env exp in
        ntimes (play_rec body) state n
    | Block seq ->
        List.fold_left (fun acc instr -> play_rec instr acc) state seq
    | Right exp ->
        let a = eval env exp in
        {state with h = state.h - a}
    | Fwd exp ->
        let d = eval env exp in
        let hrad = deg2rad state.h in
        let x = state.x + truncate (cos hrad *. float d) in
        let y = state.y + truncate (sin hrad *. float d) in
        Graphics.draw_poly_line [|(state.x, state.y); (x, y)|];
        {state with x; y} in
  ignore (play_rec prog {h = 0; x = 300; y = 225})

let _ =
  Graphics.open_graph "";
  Graphics.moveto 300 225;
  play3 ["ne", 36; "sz", 20] exemple;
  Graphics.read_key ()
