val get_opt : string -> int -> char option

val is_substring : ?from:int -> needle:string -> string -> bool

val is_prefix : ?from:int -> string -> string -> bool
val is_postfix : string -> string -> bool

val is_prefix' : ?from:int -> string -> string -> string option
(** Same as [prefix] but returns the postfix *)

val is_postfix' : string -> string -> string option
(** Same as [postfix] but returns the prefix *)

val index_from_to : string -> int -> int -> char -> int option
val index_string_from : string -> int -> string -> int (* may raise Not_found *)

val sub_from_to : string -> int -> int -> string

val chop_eols : string -> string
(** [chop_eols s] returns the string [s] w/o the end-of-line chars. [chop] from Perl.
    [chop_eols "hello\r\n" = "hello"]
    [chop_eols "hello\n" = "hello"]
    [chop_eols "hello\r" = "hello"]
    [chop_eols "hello" = "hello"]
*)

val is_space_or_tab : char -> bool
val is_newline_or_return : char -> bool

val make1 : char -> string
(** [make1 = String.make 1] *) 

val lines : string -> (string * string) list
(** [lines "hello\nworld\r\ngood\rday" = ["hello", "\n"; "world", "\r\n"; "good", "\r"; "day", ""]]  *)

val split : (char -> bool) -> string -> string list
(** [split (function ' ' -> true | _ -> false) "hello      world" = ["hello"; "world"]] *) 

module Set : Xset.S with type elt = String.t

val split1 : ?from:int -> (char -> bool) -> string -> (string * string) option
(** Same as [split] but do the split only once *)

(** Haskelish string sub *)
val split_at : int -> string -> string * string
val take : int -> string -> string
val drop : int -> string -> string
val drop_postfix : int -> string -> string

val prefix : int -> string -> string
(** same as take *)

val postfix : int -> string -> string

(** Optionalized *)
val index_opt : string -> char -> int option

val find : string -> int -> (char -> bool) -> int option

val scani_left : 
  (int -> 'a -> char -> [< `Continue of 'a | `Stop of 'a]) 
  -> 'a -> ?from:int -> ?to_:int -> string -> 'a

val foldi_left : 
  (int -> 'a -> char -> [< `Continue of 'a | `Stop of 'a]) 
  -> 'a -> string -> 'a

module Pervasives : sig
  val chop_eols : string -> string
end

val sub' : string -> int -> int -> string
(** Same as [String.sub] but even if the string shorter for [len] 
    the function succeeds and returns a shorter substring. 
*)

val replace_chars : char -> char -> string -> string
(** [replace_chars c1 c2 s] returns a copy of [s] with replacing
    all the char occurrences of [c1] by [c2]. *)

val to_array : string -> char array

val to_code_array : string -> int array

val random : int (** length *) -> string

val random_hum : int (** length *) -> string
(** human readable *)

