
(** Display on a graphic window all elements relative to the current state of a path-finding problem resolution.*)

exception Exit

type graph= {
  width: int;
  height: int }

val open_graph : Problem.problem -> graph
(** [open_graph pb] opens the graphic windows, and returns the graphic windows characteristics (width, height). *)
(* Note : It also turns off the on-screen drawing (auto-synchronize <- false) so that all drawing is made in the backing store until Graphics.synchronize is called.*)

val scale : graph -> Problem.problem -> Problem.point -> int * int
(** [scale graph problem (x,y)] returns the graphic window coordinates corresponding to coordinates [(x,y)] in the problem space. *)

val display:
  (Problem.point -> int *int) -> Problem.point -> Problem.point list
  -> Problem.point list -> Problem.point list -> Problem.problem -> unit
(** [display scale (x,y) path frontier closed pb] draws all elements relative to the current state [u] of the path-finding problem [pb] on the graphic window. *)

val wait : Graphics.event list -> unit
(** [wait events] waits for the until one of the events specified in [events] occurs, and performs the corresponding actions (e.g. : raise exception [Exit] when 'q' is key-pressed. *)

val sleep : float -> unit
(** [sleep t] waits [t] seconds. *)
