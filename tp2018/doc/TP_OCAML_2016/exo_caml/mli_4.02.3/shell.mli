val cp : string list -> Unix.process_status * unit

val mv : string list -> Unix.process_status * unit

val rm : string list -> Unix.process_status * unit

val cat : string list -> Unix.process_status * unit

val cmp : string -> string -> [`Same | `Different | `Error]
(** Execute "cmp", the file comparison unix command by execvp *)
  
val file : string -> [> `Error of Unix.process_status
                     | `Ok of string option ]
(** Execute "file path" *)

val grep : 
  string list
  -> init:'a 
  -> f: ('a -> [ `Err | `Out ] * [ `EOF | `Read of string ] -> 'a) 
  -> Unix.process_status * 'a
(** Run grep command *)

val grep_ : string list -> Unix.process_status * unit
(** Run grep command but just returns the result *)
