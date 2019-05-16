  
(* 1 *)
module type DOMAINE =
sig
  type t
  val fprint : channel_out -> expression -> unit
  val cst : float -> t
  val sum :  t -> t -> t
  val mul :  t -> t -> t
  val cond :  t -> t -> t ->
end

(* 2 *)
type t =
  | Cst
  | Var of string
  | Sum of t * t
  | Prod of t * t
  | Cond of t 

(* 3 *)
let cst (x:float):t



(*

le type t des éléments du domaine d’interprétation ;
— une fonction fprint qui prend un canal de sortie, et une expression en paramètres et
renvoie unit ;
— une fonction cst qui prend un flottant en paramètre et renvoie un élément du domaine ;
— les fonctions sum et mul qui prennent deux éléments et en renvoie un ;
— la fonction cond qui prend trois éléments et en renvoie un.

module Hello : Hello_type =
struct
  ...
end

(* F : foncteur ; X_type : signature ; X module *)
module F (X : X_type) =
struct
  ...
end

*)
