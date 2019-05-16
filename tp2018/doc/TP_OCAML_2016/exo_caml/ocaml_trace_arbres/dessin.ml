open Odot

let node lbl name =
  Stmt_node 
    ((dblq_node_id lbl), [(Simple_id "label", 
			   Some (Double_quoted_id name))])

let edge id1 id2 = Stmt_edge
  ((Edge_node_id (dblq_node_id id1)),
   [Edge_node_id (dblq_node_id id2)], [])

let draw_graphdot name = 
  let stmt_list = [node "a" "A"; node "b" "B"; 
	      node "c" "C" ; node "d" "D";
	      edge "a" "b" ; edge "c" "d" ;
	      edge "a" "c" ; edge "d" "a";
	      edge "c" "c"]
  in
  {strict=true;kind=Digraph; stmt_list ;
   id = Some (Double_quoted_id name)}
   
let () =   
  let graph = draw_graphdot "Graph" in
  Odot.print_file "graph.dot" graph;
  ignore (Sys.command "dot -Tsvg graph.dot -o graph.svg");
  ignore (Sys.command "dot -Tpng graph.dot -o graph.png")
