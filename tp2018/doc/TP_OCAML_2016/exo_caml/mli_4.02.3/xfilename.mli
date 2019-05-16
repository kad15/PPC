val split_extension : string -> string * string
(** [split_extension path] split the body and extension of [path].
    [split_extension "hello.world.txt" = "hello.world", ".txt"]
    [split_extension "hello_world" = "hello_world", ""]
*)

val is_root : string -> bool

module Pervasives : sig
  val (^/) : string -> string -> string
  (** Filename concatenation. If the second argument is absolute,
      the first is ignored and the second is just returened.

      "hello" ^/ "world" = "hello/world"
      "hello" ^/ "/world" = "/world"
  *)
end


