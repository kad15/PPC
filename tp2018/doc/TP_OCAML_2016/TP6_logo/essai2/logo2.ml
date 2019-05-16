
type expression =
    Cst of int
  | Var of string
  | Div of expression * expression

(*2. Représenter l’expression (360 / ne) avec ce type.*)
let ex = Div (Cst 360, Var "ne");;

(* 3 *)
type instruction =
    Repeat of expression * instruction
  | Block of instruction list
  | Right of expression
  | Fwd of expression

(* 4 Représenter le programme exemple donné ci-dessus avec ce type. *)
(* 

repeat ne [right (360 / ne); repeat ne [right (360 / ne); forward sz]] 

*)
let inst1 = Right ex;;
let inst2 = Repeat (Var "ne", Right ex);;
let inst3 = Fwd (Var "sz");;
let prog = Block  [inst1;inst2;inst3];;

(*

let exemple = Block [Right (Div (Cst 360, Var "ne"));
    Repeat (Var "ne", Right (Div (Cst 360, Var "ne"))); Fwd (Var "sz")]

*)

let exemple = Repeat (Var "ne",
                      Block [Right ex;
                             Repeat (Var "ne",
                                     Block [Right ex;
                                            Fwd (Var "sz")])]);;

(* 5

Pour prendre en compte les variables, on utilise un environnement associant à chaque variable une valeur entière. 
On pourra le représenter par une liste de couples (variable, valeur) 
et utiliser la fonction List.assoc pour y accéder. Écrire la fonction :
val eval : (string * int) list -> expression -> int

*)
let eval env exp =
  let rec aux e = 
   match e with
   | Cst c -> c
   | Var id -> List.assoc id env
   | Div (a1,a2) -> aux a1 / aux a2 in 
   aux exp;; 



(* 6  
val rlineto : int -> int -> unit
Graphics.rlineto dx dy dessine un segment depuis le point courant jusqu’au point courant translaté de
(dx, dy) (voir la figure 1). Nous allons utiliser également un cap courant, implémenté par une référence
locale (ici un cap en degrés entier) :
Écrire la fonction play qui interprète une instruction Logo :
val play : (string * int) list -> instruction -> unit

repeat ne [right (360 / ne);
repeat ne [right (360 / ne); forward sz]]
*)

let pi = acos (-1.)
let rad_of_deg d =  pi *. float d /. 180.
let deg2rad = rad_of_deg;;
module G = Graphics;;
module L = List;;

let play env prog =
  let heading = ref 0 in
  let rec play_rec instr =
  match instr with 
  | Repeat (exp_nb_rep,inst_a_repeter) -> 
     let n = eval env exp_nb_rep in
     for i = 1 to n do  play_rec inst_a_repeter  done
  | Block listOfInstructions -> L.iter play_rec listOfInstructions
  | Right exp_angle_deg  -> 
     let a = eval env exp_angle_deg in 
     heading := !heading - a 
  | Fwd exp_distance  ->
     let d = eval env exp_distance in
     let a = rad_of_deg !heading in 
     let dx = truncate(float d *. cos a) in
     let dy = truncate(float d *. sin a) in
     G.rlineto dx dy in 
  play_rec prog;;


let _ =
  Graphics.open_graph "";
  Graphics.moveto 300 225;
  play ["ne", 36; "sz", 20] exemple;
  Graphics.read_key ()

