(** Some simple file content iterations and 
    /usr/bin/test style file status checking functions. *)

open Base

(** {6 Iterators over lines } *)

val iter_lines_exn : string -> (string -> 'a) -> unit
(** [iter_lines_exn filename f] iters [f] over lines of contets of [filename] *)

val iter_lines : string -> (string -> unit) -> (unit, [> `Exn of exn]) result
(** [iter_lines filename f] iters [f] over lines of contets of [filename] *)

val to_lines : string -> (string list, [> `Exn of exn]) result
(** [iter_lines filename f] iters [f] over lines of contets of [filename] *)

val to_string : string -> (string, [> `Exn of exn]) result
(** [to_string filename] returns the contens of the file *)

val open_out : string -> (out_channel -> 'a) -> 'a
(** [open_out filename f] opens [filename] then runs [f] over the opened channel. 
    The channel is closed automatically no matter whether [f] terminates normally
    or raises an exception.
*)

val write_lines : string -> string list -> unit

(** /usr/bin/test like file testing *)
module Test : sig
  type test_unary = 
      string  (** file name *)
      -> [ `TRUE  of Unix.stats
	 | `FALSE of Unix.stats
  	 | `Error of Unix.error ]
	    
  (*     FILE1 -ef FILE2
                FILE1 and FILE2 have the same device and inode numbers
  
         FILE1 -nt FILE2
                FILE1 is newer (modification date) than FILE2
  
         FILE1 -ot FILE2
                FILE1 is older than FILE2
  
  val _b : string -> bool (* FILE exists and is block special *)
  val _b' : test_unary (* FILE exists and is block special *)
  val _c : string -> bool (* FILE exists and is character special *)
  val _c' : test_unary (* FILE exists and is character special *)
  *)
  
  val _d : string -> bool
  val _d' : test_unary 
  (** -d: FILE exists and is a directory *)

  val _e : string -> bool
  val _e' : string -> 
    [ `TRUE  of Unix.stats
    | `FALSE
    | `Error of Unix.error ]
  (** -e: FILE exists *)

  val _f : string -> bool
  val _f' : test_unary 
  (** -f: FILE exists and is a regular file *)

  (*
  val _g : string -> bool (* FILE exists and is set-group-ID *)
  val _g' : test_unary (* FILE exists and is set-group-ID *)
  val _G : string -> bool (* FILE exists and is owned by the effective group ID *)
  val _G' : test_unary (* FILE exists and is owned by the effective group ID *)
  *)

  val _h : string -> bool
  val _h' : test_unary 
  (** -h: FILE exists and is a symbolic link (same as -L) *)

  (*
  val _k : string -> bool (* FILE exists and has its sticky bit set *)
  val _k' : test_unary (* FILE exists and has its sticky bit set *)
  *)

  val _L : string -> bool
  val _L' : test_unary 
  (** -L: FILE exists and is a symbolic link (same as -h) *)

  (*
  val _O : string -> bool (* FILE exists and is owned by the effective user ID *)
  val _O' : test_unary (* FILE exists and is owned by the effective user ID *)
  val _p : string -> bool (* FILE exists and is a named pipe*)
  val _p' : test_unary (* FILE exists and is a named pipe*)
  val _r : string -> bool (* FILE exists and read permission is granted*)
  val _r' : test_unary (* FILE exists and read permission is granted*)
  *)

  val _s : string -> bool
  val _s' : test_unary 
  (** -s: FILE exists and has a size greater than zero*)

  (*
  val _S : string -> bool (* FILE exists and is a socket*)
  val _S' : test_unary (* FILE exists and is a socket*)
  
  val _t : string -> bool (* file descriptor FD is opened on a terminal *)
  val _t' : test_unary (* file descriptor FD is opened on a terminal *)
  		       IT TAKES FD not FILE!
  
  val _u : string -> bool (* FILE exists and its set-user-ID bit is set*)
  val _u' : test_unary (* FILE exists and its set-user-ID bit is set*)
  val _w : string -> bool (* FILE exists and write permission is granted*)
  val _w' : test_unary (* FILE exists and write permission is granted*)
  val _x : string -> bool (* FILE exists and execute (or search) permission is granted*)
  val _x' : test_unary (* FILE exists and execute (or search) permission is granted*)
  *)
end
