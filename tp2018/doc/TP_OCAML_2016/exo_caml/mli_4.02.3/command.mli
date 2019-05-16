val liner : ?cut_threshold:int -> unit -> bool -> string -> string list

val multi_stream_read :
  (Unix.file_descr 
   * ([ `EOF | `Error of exn | `Read of string ] -> 'res) 
   * (bool -> string -> string list)) list 
  -> 'res Stream.t

val raw_exec :
  ?env:string array 
  -> string 
  -> ([ `Err of [ `EOF | `Error of exn | `Read of string ]
      | `Out of [ `EOF | `Error of exn | `Read of string ] ] Stream.t
      * (unit -> Unix.process_status))

val exec :
  ?env:string array  (** env strings *)
  -> string list (** command string tokens *)
  -> (Unix.process_status -> [> ('a, [> `Exn of exn]) Result.t ] -> 'res) (** process finalizer *)
  -> ([ `Err of [ `EOF | `Error of exn | `Read of string ]
      | `Out of [ `EOF | `Error of exn | `Read of string ] ] Stream.t -> 'a) (** stream processing *)
  -> 'res

val shell_exec :
  ?env:string array  (** env strings *)
  -> string  (** command string *)
  -> (Unix.process_status -> [> ('a, [> `Exn of exn]) Result.t ] -> 'res) (** process finalizer *)
  -> ([ `Err of [ `EOF | `Error of exn | `Read of string ]
      | `Out of [ `EOF | `Error of exn | `Read of string ] ] Stream.t -> 'a) (** stream processing *)
  -> 'res

val must_exit_with : int -> Unix.process_status -> ('a, [ `Exn of exn ]) Result.t -> 'a
(** a process finalizer. If the exit status is different from the argument,
    it raises [Failure mes]
*)

val force_lines : 
  [ `Err of [ `EOF | `Error of exn | `Read of string ]
  | `Out of [ `EOF | `Error of exn | `Read of string ] ] Stream.t -> string list

val force_stdout : 
  [ `Err of [ `EOF | `Error of exn | `Read of string ]
  | `Out of [ `EOF | `Error of exn | `Read of string ] ] Stream.t -> string list

val force_stderr : 
  [ `Err of [ `EOF | `Error of exn | `Read of string ]
  | `Out of [ `EOF | `Error of exn | `Read of string ] ] Stream.t -> string list
