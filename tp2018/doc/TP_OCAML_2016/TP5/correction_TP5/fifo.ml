type 'a t = 'a list * 'a list

exception Empty

let empty = ([], [])

let is_empty = fun q ->
  q = empty

let add = fun x (front, back) ->
  (x::front, back)

(*
let take = fun q ->
  match q with
    (front, []) ->
      begin
        match List.rev front with
          [] -> raise Empty
        | x :: xs -> (x, ([], xs))
      end
  | (front, x :: back) -> (x, (front, back))
*)

let rec take = fun q ->
  match q with
    ([], []) -> raise Empty
  | (front, []) -> take ([], List.rev front)
  | (front, x :: back) -> (x, (front, back))
