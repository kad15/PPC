type cell = Free | Wall | Exit | Marked | Dead | Path
(* Type of cells. *)

type t
(* Type of mazes. *)

val read : string -> t
(* [read file] returns the maze described in [file]. *)

val init : t -> unit
(* [init maze] initializes the graphic window. *)

val get : t -> int -> int -> cell
(* [get maze x y] returns the cell of [maze] at coordinates ([x],[y]). *)

val start : t -> int * int
(* [start maze] returns the coordinates of the starting point. *)

val mark : t -> int -> int -> unit
val path : t -> int -> int -> unit
val dead : t -> int -> int -> unit
(* [mark|path|dead maze x y] updates [maze] and the graphic window
   at coordinates ([x],[y]). *)

val sleep : float -> unit
(* [sleep s] sleeps for [s] seconds. *)

val wait_key : unit -> unit
(* [wait_key ()] waits until a key is pressed. *)
