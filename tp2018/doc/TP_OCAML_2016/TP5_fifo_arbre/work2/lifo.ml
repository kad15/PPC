type 'a t = {stack : 'a list; size : int}

exception Empty

let empty = {stack = []; size = 0}

let is_empty s =
  s.size = 0

let add x s =
  {stack = x :: s.stack; size = s.size + 1}

let take s =
  match s.stack with
    [] -> raise Empty
  | x :: xs -> (x, {stack = xs; size = s.size - 1})

let size s = s.size

let iter f s = List.iter f s.stack



