type 'a t =
    Empty
  | Node of 'a t * 'a * 'a t

module Set = Lifo

let iter = fun f t ->
  let rec iter_rec = fun nodes ->
    if not (Set.is_empty nodes) then
      begin
        let (node, nodes) = Set.take nodes in
        match node with
          Empty -> iter_rec nodes
        | Node (l, x, r) ->
            f x;
            iter_rec (Set.add r (Set.add l nodes))
      end in
  iter_rec (Set.add t Set.empty)

let t = Node (Node (Node (Empty, 4, Empty), 2, Node (Empty, 5, Empty)),
              1,
              Node (Node (Empty, 6, Empty), 3, Empty))

let () =
  iter (fun x -> Printf.printf "%d " x) t;
  Printf.printf "\n"


