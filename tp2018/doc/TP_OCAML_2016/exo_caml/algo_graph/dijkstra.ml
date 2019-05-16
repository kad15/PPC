(* Plus court chemin : algorithme de Dijkstra *)
open Hashtbl;;

  type graph
  type vertex
  (*val iter_vertex: (vertex -> unit) -> graph -> unit
  val iter_succ: (vertex -> unit) -> graph -> vertex -> unit*)
  module H: HashTable with type key = vertex




  module VertexDistance = struct
    type t = vertex * float
    let compare (_, d1) (_, d2) = Pervasives.compare d1 d2
  end
  module P = PriorityQueue(VertexDistance)

  let rec dijkstra f g s =
    let visited = H.create () in
    let distance = H.create () in
    let queue = P.create () in
    let add v d = H.replace distance v d; P.add queue (v, d) in
    add s 0.;
    while not (P.is_empty queue) do
      let (u, du) = P.extract_min queue in
      if not (H.mem visited u) then begin
        H.add visited u ();
        f u du;
        let visit v =
          let d = du +. weight g u v in
          if not (H.mem distance v) || d < H.find distance v then
            add v d
        in
        iter_succ visit g u
      end
    done
