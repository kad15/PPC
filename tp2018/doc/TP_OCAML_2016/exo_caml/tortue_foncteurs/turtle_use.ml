open Turtle;;
module G = Graphics;;
(* pour utiliser notre module foncteur Turtle, il faut commencer par se donner
un module particulier d'interface ANGLE.

si on fait le choix d'un module représentant un un angles
en radians et par un flottant, on peut definier un tel module Angle' *)

module Angle:ANGLE = struct
type t  = float
let add = (+.)
let pi_over_180 = atan 1./.45.
let of_degrees d = d *. pi_over_180
let cos = Pervasives.cos
let sin = Pervasives.sin
end

(*on obtient alors un module T en appliquant le focnteur Turtle au modukle Angle
ce qui s'écrit'*)

module T = Turtle(Angle)

(*on peut enfin utiliser le module T pour dessiner une figure en écriavant par exemple*)

(*dessine un carré de coté d*)
let square d = 
 for k = 1 to 4 do T.advance d; T.rotate_left 90. done


(*dessine des carrés de coté d tourné de a degrés entre chaque *)
let squares d a =
for k = 1 to truncate(360./.a) do
  square d; T.rotate_left a
done;;


let ()  = squares 100. 20. ; ignore (G.read_key());;

(*l'interet d'avoir écrit le module Turtle comem un foncteur
paramétré par une représetnation d'angle est que nous pouvons l'appliquer une seconde fois
 à un autre module de signature ANGLE et obtenir ainsi une autre tortue où les angles
sont représenté différemment' '*)





