type 'a t = Empty | Node of 'a t * 'a * 'a t;;

module Set = Lifo


let iter f t =
  let rec aux nodes =
    if not (Set.is_empty nodes) then
      (
        let (node, nodes) = Set.take nodes in
        match node with
        | Empty -> aux nodes
        | Node (l, x, r) ->
            f x; aux (Set.add r (Set.add l nodes))
      ) 
    in
    aux (Set.add t Set.empty)



let tree = Node(Node(Node(Empty,4,Empty),2,Node(Empty,5,Empty)), 1, 
           Node(Node(Empty,6,Empty),3,Empty));;

let () =
  iter (fun x -> Printf.printf "%d " x) tree;
  Printf.printf "\n"
