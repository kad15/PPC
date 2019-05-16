type 'a t = {stack : 'a list; size : int}

exception Empty

let empty = {stack = []; size = 0}

let is_empty = fun s ->
  s.size = 0

let add = fun x s ->
  {stack = x :: s.stack; size = s.size + 1}

let take = fun s ->
  match s.stack with
    [] -> raise Empty
  | x :: xs -> (x, {stack = xs; size = s.size - 1})

let size = fun s ->
  s.size

let iter = fun f s ->
  List.iter f s.stack



