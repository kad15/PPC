
(** A path-finding problem, with an obstacle between origin and destination. *)

type point = int * int
(** A point is represented by its coordinates [(x,y)] *)

type obstacle=
  Rectangle of point*point

type problem = {
  xmin:int;xmax:int;ymin:int;ymax:int;
  orig : point; (** Point of departure *)
  dest: point; (** Destination *)
  grid: bool array array
   (** [grid.(x).(y)] are set to [false] when an obstacle overlaps
       coordinates [(x,y)] *);
  obstacles: obstacle list }

val init_problem : unit -> problem
(** [init_problem ()] returns a path-finding problem. *)

val chars_of : problem -> char array array
(** [chars_of problem] returns a matrix or characters representing the grid. *)

val print : char array array -> unit
(** [print m] prints a matrix of characters on standard output. *)


